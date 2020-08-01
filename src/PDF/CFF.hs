{-# LANGUAGE OverloadedStrings #-}

module PDF.CFF (encoding) where

import Numeric (readInt)
import Data.Char (chr)

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

data Table = Table String Integer Integer
  deriving (Show)

data EncRecord = EncRecord Integer Integer Integer
  deriving (Show)

test f = do
  c <- BS.readFile f
  return $ encoding c

encoding :: ByteString -> Encoding
encoding c = case parseOnly stringInd c of
  Right ss -> Encoding $ zip (parseTopDictInd c) ss
  Left e -> error "Can not find String INDEX in CFF file"

parseTopDictInd :: ByteString -> [Char]
parseTopDictInd c = case parseOnly (header >> index >> index) c of
  Right ds -> concat $ map (parseDict c) ds
  Left e -> error "Can not find Top DICT INDEX"

parseDict :: ByteString -> ByteString -> [Char]
parseDict c d = case parseOnly (many1 dict) d of
  Right dictData -> case lookup [16] dictData of
    Just (DictInt n:[]) -> case parseOnly encodingArray $ BS.drop n c of
      Right arr -> map (chr . fromInteger) arr
      Left e -> error "Failed to parse Encoding Array"
    Just a -> error (show a)
    Nothing -> error "No Encodind Array"

stringInd = map (('/':) . BSC.unpack) <$> (header >> index >> index *> index)

encodingArray :: Parser [Integer]
encodingArray = do
  format <- getCard 1
  p <- fromInteger <$> getCard 1
  encodeObj format (p - 1) -- ?

  where
    encodeObj :: Integer -> Int -> Parser [Integer]
    encodeObj 0 p = count p (getCard 1)
    encodeObj 1 p = concat <$> count p getRange1

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

fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b
