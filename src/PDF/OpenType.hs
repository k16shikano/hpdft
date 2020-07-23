{-# LANGUAGE OverloadedStrings #-}

module PDF.OpenType
       where

import Numeric (readInt)
import Data.Char (chr)

import Data.Word
import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

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
  let bs = cmap c
  return bs

cmap c = case parseOnly (offsetTable >>= tableRecords) c of
  Right b -> let b' = (takeCmap b)
             in case parseOnly cmapEncRecords b' of
                  Right records -> map (subtable b') records
                  Left e -> error e
  Left e -> error e
  where
    offsetTable = do
      sfntVersion
      n <- numTables
      searchRange >> entrySelector >> rangeShift
      return $ fromIntegral n

    takeCmap ((Table "cmap" start end):_)
      = BS.take (fromInteger end) $ BS.drop (fromInteger start) c
    takeCmap (_:rest) = takeCmap rest
    takeCmap [] = error "no cmap"

    cmapEncRecords =
      cmapVersion >>
      numEncRecords >>=
      (encodeRecords . fromIntegral)
      
subtable c (EncRecord pid eid offset) =
  let body = BS.drop (fromInteger offset) c
      format = fromBytes $ BS.take 2 body
  in case parseOnly (parserByFormat format) body of
       Right b -> b
       Left e -> error e

parserByFormat :: Integer -> Parser CMap
parserByFormat 14 = do
  format <- getUint16
  length <- getUint32
  rest <- (AP.take . fromInteger) length
  return [(2,"hoge")]

parserByFormat 12 = do
  format <- getUint16
  reserved <- getUint16
  length <- getUint32
  language <- getUint32
  numGroups <- fromInteger <$> getUint32
  seqMapGroups <- count (numGroups) seqMapGroup
  return $ concat seqMapGroups

  where
    seqMapGroup :: Parser CMap
    seqMapGroup = do
      startCharCode <- fromInteger <$> getUint32
      endCharCode <- fromInteger <$> getUint32
      startGlyphID  <- fromInteger <$> getUint32
      return $ toCmap startGlyphID [startCharCode .. endCharCode]

    toCmap gid range = zip [gid ..] $ map ((:[]).chr) range


parserByFormat 4 = do
  format <- getUint16
  length <- getUint16
  rest <- (AP.take . fromInteger) length
  return [(1,"fuga")]

-- main tables

sfntVersion :: Parser ByteString
sfntVersion = contiguous [0x00, 0x01, 0x00, 0x00] <|> string "OTTO"

numTables =  getUint16
searchRange = getUint16
entrySelector = getUint16
rangeShift = getUint16

tableRecords n = count n tableRecord

tableRecord :: Parser Table
tableRecord = do
  tableTag <- BSC.unpack <$> AP.take 4
  checkSum <- getUint32
  offset <- getUint32
  length <- getUint32
  return $ Table tableTag offset length

getUint16 :: Parser Integer
getUint16 = fromBytes <$> AP.take 2

getUint32 :: Parser Integer
getUint32 = fromBytes <$> AP.take 4

tableTag :: Parser String
tableTag = BSC.unpack <$> AP.take 4

-- subtables

cmapVersion = getUint16
numEncRecords = getUint16

encodeRecords n = count n encodeRecord

encodeRecord :: Parser EncRecord
encodeRecord = do
  platformID <- getUint16
  encodingID <- getUint16
  offset <- getUint32
  return $ EncRecord platformID encodingID offset


fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

contiguous :: [Word8] -> Parser ByteString
contiguous bs = BS.pack <$> contiguous' bs
  where
    contiguous' (b:[]) = (:[]) <$> word8 b
    contiguous' (b:bs) = do
      byte <- word8 b
      rest <- contiguous' bs
      return $ (byte:rest)

