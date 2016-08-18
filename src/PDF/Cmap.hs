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

parseCMap :: BSL.ByteString -> CMap
parseCMap str = case runParser (concat <$> 
                                manyTill 
                                (try bfchar <|> (concat <$> bfrange)) 
                                (try $ string "endcmap")) () "" str of
  Left err -> error "Can not parse CMap"
  Right cmap -> cmap 

bfchar :: Parser CMap
bfchar = do
  spaces
  manyTill anyChar (try $ string "beginbfchar")
  spaces
  ms <- many1 (toCmap <$> hexletters <*> hexletters)
  spaces
  string "endbfchar"
  spaces
  return ms
    where toCmap cid ucs = ((fst.head.readHex) cid, ((:[]).chr.fst.head.readHex) ucs)

bfrange :: Parser [CMap]
bfrange = do
  spaces
  manyTill anyChar (try $ string "beginbfrange")
  spaces
  ms <- many1 (toCmap <$> (getRange <$> hexletters <*> hexletters) <*> hexletters)
  spaces
  string "endbfrange"
  spaces
  return ms
    where 
      getRange cid cid' = [gethex cid .. gethex cid']
      toCmap range ucs = zip range (map ((:[]).chr) [gethex ucs ..])

gethex = fst.head.readHex


hexletters :: Parser String
hexletters = do
  char '<'
  lets <- manyTill hexletter (try $ char '>')
  spaces
  return $ concat lets

hexletter :: Parser String
hexletter = (count 4 $ oneOf "0123456789ABCDEFabcdef")
