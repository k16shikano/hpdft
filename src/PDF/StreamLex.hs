{-# LANGUAGE OverloadedStrings #-}

module PDF.StreamLex
  ( normalizePdfNumber
  , parsePdfNumber
  , parsePdfNumberFromByteString
  , hexPairs
  , isUtf16HighSurrogate
  , isUtf16LowSurrogate
  , surrogatePairToCode
  , unicodeBytesToCodes
  , isSjisLead
  , sjisBytesToCodes
  , jisBytesToCodes
  ) where

import Data.Bits (shiftL)
import Data.Char (chr)
import qualified Data.ByteString.Lazy as BSL
import Numeric (readHex)

normalizePdfNumber :: String -> String
normalizePdfNumber s
  | null s = s
  | head s == '.' = '0' : s
  | length s >= 2 && head s == '-' && s !! 1 == '.' = '-' : '0' : drop 1 s
  | otherwise = s

parsePdfNumber :: String -> Double
parsePdfNumber s
  | null s || s == "-" || s == "+" = 0
  | last s == '.' =
      case reads (normalizePdfNumber s ++ "0") of
        [(n, "")] -> n
        _         -> 0
  | otherwise =
      case reads (normalizePdfNumber s) of
        [(n, "")] -> n
        _         -> 0

parsePdfNumberFromByteString :: BSL.ByteString -> Double
parsePdfNumberFromByteString bs =
  parsePdfNumber (map (chr . fromEnum) (BSL.unpack bs))

hexPairs :: String -> [Int]
hexPairs [] = []
hexPairs [x] =
  case readHex [x, '0'] of
    [(n, "")] -> [n]
    _         -> []
hexPairs (a:b:rest) =
  case readHex [a, b] of
    [(n, "")] -> n : hexPairs rest
    _         -> hexPairs rest

isUtf16HighSurrogate :: Int -> Bool
isUtf16HighSurrogate u = u >= 0xD800 && u <= 0xDBFF

isUtf16LowSurrogate :: Int -> Bool
isUtf16LowSurrogate u = u >= 0xDC00 && u <= 0xDFFF

surrogatePairToCode :: Int -> Int -> Int
surrogatePairToCode hi lo = 0x10000 + ((hi - 0xD800) `shiftL` 10) + (lo - 0xDC00)

unicodeBytesToCodes :: [Int] -> [Int]
unicodeBytesToCodes [] = []
unicodeBytesToCodes [_] = []
unicodeBytesToCodes (a:b:rest) =
  let unit = a * 256 + b
  in if isUtf16HighSurrogate unit
     then case rest of
       (c:d:rs) ->
         let unit2 = c * 256 + d
         in if isUtf16LowSurrogate unit2
            then surrogatePairToCode unit unit2 : unicodeBytesToCodes rs
            else unit : unicodeBytesToCodes rest
       _ -> [unit]
     else unit : unicodeBytesToCodes rest

isSjisLead :: Int -> Bool
isSjisLead b = (b >= 0x81 && b <= 0x9F) || (b >= 0xE0 && b <= 0xFC)

sjisBytesToCodes :: [Int] -> [Int]
sjisBytesToCodes [] = []
sjisBytesToCodes (b:rest)
  | isSjisLead b = case rest of
      (t:rs) -> (b * 256 + t) : sjisBytesToCodes rs
      _ -> [b]
  | otherwise = b : sjisBytesToCodes rest

jisBytesToCodes :: [Int] -> [Int]
jisBytesToCodes [] = []
jisBytesToCodes [_] = []
jisBytesToCodes (a:b:rest) = (a * 256 + b) : jisBytesToCodes rest
