{-# LANGUAGE OverloadedStrings #-}

module PDF.CFF (encoding) where

import Numeric (readInt)
import Data.Char (chr, intToDigit)
import Data.List (isPrefixOf)

import Data.Word
import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

import Data.Attoparsec.ByteString (Parser, parseOnly, word8, string)
import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.Combinator

import Control.Applicative

import Debug.Trace

import PDF.Definition

test f = do
  c <- BS.readFile f
  return $ encoding c

encoding :: ByteString -> Encoding
encoding c =
  Encoding $ map findEncodings $ zip charset encodings

  where
    ds = parseTopDictInd c
    encodings  = concatMap (parseEncoding c) ds
    charset = concatMap (parseCharset c) ds
    fontname = concat $ map (parseFontname c) ds         
    strings = case parseOnly stringInd c of
                Right arr -> arr
                Left e -> error "Failed to parse STRING Index"

    findEncodings :: (Integer, Char) -> (Char, String)
    findEncodings (char,enc) =
      case char of
        s | s > 390 -> (enc, stringToText $ strings !! fromInteger (char - 390 - 1))
          | s > 95 -> (enc, sidToText s) 
          | otherwise -> (enc, enc:[])

    -- defined in String INDEX of each font
    stringToText "a113" = "‡"
    stringToText "a114" = "・"
    stringToText "trianglesolid" = "▲"
    stringToText x = "[CFF:String " <> x <> "]"

    -- pre-defined in Appendix C of CFF specs
    sidToText 112 = "†"
    sidToText 166 = "－"
    sidToText n = "[CFF:SID " <> (show n) <> "]"

parseTopDictInd :: ByteString -> [ByteString]
parseTopDictInd c = case parseOnly (header >> index *> index) c of
  Right ds -> ds
  Left e -> error "Can not find Top DICT INDEX"

parseEncoding :: ByteString -> ByteString -> [Char]
parseEncoding c d = case parseOnly (many1 dict) d of
  -- '16' is key for 'Encoding' in Top DICT
  Right dictData -> case lookup [16] dictData of 
    Just (DictInt 0:[]) -> [] -- Standard Encoding (not supported)
    Just (DictInt 1:[]) -> [] -- Expert Encoding (not supported)
    Just (DictInt n:[]) -> -- n is offset
      case parseOnly encodingArray $ BS.drop n c of
        Right arr -> map (chr . fromInteger) arr
        Left e -> error "Failed to parse Encoding Array"
    Just a -> error (show a)
    Nothing -> error $ "No Encodind Array in " ++ show dictData
  Left _ -> error "Failed to parse Top DICT in CFF"

parseFontname c d = case parseOnly (many1 dict) d of
  Right dictData -> case lookup [2] dictData of
    Just (DictInt n:[]) ->
      case parseOnly stringInd c of
        Right arr -> arr !! (n - 390 - 1) -- 390 seems to be a magic number
        Left e -> error "Failed to parse Fontname"
    Just a -> error (show a)
    Nothing -> error $ "No Fontname in " <> show dictData
  Left _ -> error "Failed to parse Top DICT in CFF"

parseCharset c d = case parseOnly (many1 dict) d of
  Right dictData -> case lookup [15] dictData of
    Just (DictInt offset:[]) -> 
      case parseOnly (charsetData $ charStringsInd c dictData) $
           BS.drop offset c of
        Right arr -> arr
        Left _ -> error ""
    Just a -> error (show a)
    Nothing -> error $ "No Charset in " <> show dictData
  Left _ -> error "Failed to parse Top DICT in CFF"

charStringsInd c dictData = 
  case lookup [17] dictData of
    Just (DictInt offset:[]) -> 
      case parseOnly index (BS.drop offset c) of
        Right [] -> error "failed to get CharStrings"
        Right ind -> ind
        Left "" -> error "failed to get CharStrings"
    Nothing -> error $ "No CharStrings in " <> show dictData

nameInd = map BSC.unpack <$> (header *> index)

dictInd = BSC.concat <$> (header >> index *> index)

stringInd = map BSC.unpack <$> (header >> index >> index *> index)

charsetData :: [ByteString] -> Parser [Integer]
charsetData ind = do
  format <- getCard 1
  charsetObj format
  
  where
    -- .notdef must be excluded, so minus one
    charsetObj 0 = count (length ind - 1) getSID 


encodingArray :: Parser [Integer]
encodingArray = do
  format <- getCard 1
  p <- fromInteger <$> getCard 1
  encodeObj format p

  where
    encodeObj :: Integer -> Int -> Parser [Integer]
    encodeObj 0 p = count (p - 1) (getCard 1)
    encodeObj 1 p = concat <$> count p getRange1
    encodeObj _ p = error "CFF Supplement Format is not supported."

    getRange1 :: Parser [Integer]
    getRange1 = do
      first <- getCard 1
      nleft <- getCard 1
      return $ [first .. first + nleft]

data DictOp = DictInt Int | DictReal Double | SID ByteString
  deriving (Show)

dict = (flip (,)) <$> (manyTill dictOp (try $ lookAhead dictKey)) <*> dictKey

dictOp :: Parser DictOp
dictOp = do
  b0 <- fromInteger <$> getCard 1
  opEnc b0
  
  where
    opEnc :: Int -> Parser DictOp
    opEnc b0 | b0 >= 32 && b0 <= 246 = return $ DictInt $ b0 - 139
             | b0 >= 247 && b0 <= 250 = do
                 b1 <- fromInteger <$> getCard 1
                 return $ DictInt $ (b0 - 247) * 256 + b1 + 108
             | b0 >= 251 && b0 <= 254 = do
                 b1 <- fromInteger <$> getCard 1
                 return $ DictInt $ - (b0 - 251) * 256 - b1 - 108
             | b0 == 28 = do
                 b1 <- fromInteger <$> getCard 1
                 b2 <- fromInteger <$> getCard 1
                 return $ DictInt $ b1 `shiftL` 8 .|. b2
             | b0 == 29 = do
                 b1 <- fromInteger <$> getCard 1
                 b2 <- fromInteger <$> getCard 1
                 b3 <- fromInteger <$> getCard 1
                 b4 <- fromInteger <$> getCard 1
                 return $ DictInt $  b1 `shiftL` 24 .|. b2 `shiftL` 16 .|. b3 `shiftL` 8 .|. b4
             | b0 == 30 = do
                 r <- many1 $ AP.satisfy (\w -> (240 .|. w) `xor` 255 /= 0)
                 f <- getCard 1
                 return $ DictReal $ readNibble r f
             | otherwise = error (show b0)
    readNibble s1 s2 = 0

dictKey :: Parser [Word8]
dictKey = do
  key <- AP.choice [ try $ many1 $ AP.satisfy $ AP.inClass "\0-\5\13-\18"
                   , try $ (flip (flip (:) . (:[])))
                     <$> AP.satisfy (==12) <*> (AP.satisfy $ AP.inClass "\0-\8\20-\23\30-\38")
                   ]
  return key

index :: Parser [ByteString]
index = do
  indexCount <- fromInteger <$> getCard 2
  offSize <- fromInteger <$> getCard 1
  offsets <- map fromInteger <$> count (indexCount+1) (getCard offSize)
  indexData <- repeatFor offsets
  return indexData
  where
    repeatFor ls = sequence $ map AP.take $ differenciate ls
    differenciate ls = tail $ zipWith subtract (0:ls) ls

header :: Parser Integer
header = do
  major <- getCard 1
  minor <- getCard 1
  hrdSize <- getCard 1
  offSize <- getCard 1
  return $ major

getCard :: Int -> Parser Integer
getCard n = fromBytes <$> AP.take n

getSID :: Parser Integer
getSID = fromBytes <$> AP.take 2

fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
