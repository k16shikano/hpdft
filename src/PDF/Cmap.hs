{-# LANGUAGE OverloadedStrings #-}

module PDF.Cmap
       ( parseCMap
       ) where

import Data.Char (chr)
import Numeric (readOct, readHex)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import Text.Parsec.ByteString.Lazy
import Codec.Compression.Zlib (decompress) 

import Debug.Trace

import PDF.Definition
import PDF.Character (pdfchardict)

parseCMap :: BSL.ByteString -> CMap
parseCMap str = case runParser (concat <$> manyTill cmapParser (try $ string "endcmap")) () "" str of
  Left err -> error "Can not parse CMap"
  Right cmap -> cmap 

cmapParser :: Parser CMap
cmapParser = do
  spaces
  manyTill anyChar (try $ string "beginbfchar")
  spaces
  ms <- many1 (toCmap <$> hexletters <*> hexletters)
  spaces
  string "endbfchar"
  spaces
  return ms
    where toCmap cid ucs = ((fst.head.readHex) cid, ((:[]).chr.fst.head.readHex) ucs)

hexletters :: Parser String
hexletters = do
  char '<'
  lets <- manyTill hexletter (try $ char '>')
  spaces
  return $ concat lets

hexletter :: Parser String
hexletter = (count 4 $ oneOf "0123456789ABCDEFabcdef")
