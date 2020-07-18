{-# LANGUAGE OverloadedStrings #-}

module PDF.Cmap
       ( parseCMap
       ) where

import Data.Char (chr)
import Data.List (intercalate)
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
parseCMap str = case runParser (skipHeader >>
                                concat <$>
                                manyTill
                                 (choice
                                 [ try bfchar
                                 , try $ concat <$> bfrange
                                 ])
                                 (try $ string "endcmap"))
                               () "" str of
  Left err -> error $ "Can not parse CMap " ++ (show err)
  Right cmap -> cmap

skipHeader :: Parser ()
skipHeader = do
  manyTill anyChar (try $ string "endcodespacerange")
  spaces
  return ()

bfchar :: Parser CMap
bfchar = do
  many1 digit
  spaces 
  string "beginbfchar"
  spaces
  ms <- many (toCmap <$> hexletters <*> hexletters)
  spaces
  string "endbfchar"
  spaces
  return ms
    where toCmap cid ucs = ((fst.head.readHex) cid, ((:[]).chr.fst.head.readHex) ucs)

bfrange :: Parser [CMap]
bfrange = do
  many1 digit
  spaces 
  string "beginbfrange"
  spaces
  ms <- many (toCmap <$> (getRange <$> hexletters <*> hexletters) <*> (hexletters <|> hexletterArray))
  spaces
  string "endbfrange"
  spaces
  return $ ms
    where 
      gethex = fst.head.readHex
      getRange cid cid' = [gethex cid .. gethex cid']
      toCmap range ucs = zip range (map ((:[]).chr) [gethex ucs ..])


hexletters :: Parser String
hexletters = do
  char '<'
  lets <- choice [ try $ manyTill (count 4 $ hexletter) (try $ char '>')
                 , (:[]) <$> (count 2 $ hexletter) <* char '>'
                 ]
  spaces
  return $ concat lets

hexletter :: Parser Char
hexletter = oneOf "0123456789ABCDEFabcdef"

hexletterArray :: Parser String
hexletterArray = do
  char '['
  spaces
  lets <- manyTill hexletters (try $ spaces >> char ']')
  spaces
  return $ intercalate "\n" lets

