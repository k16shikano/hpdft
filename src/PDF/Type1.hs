{-# LANGUAGE OverloadedStrings #-}

module PDF.Type1 (encoding) where

import Numeric (readInt)
import Data.Char (chr)

import Data.Word
import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.Combinator

import Control.Applicative

import qualified Data.Map as Map
import qualified Data.Text as T

import PDF.Definition

test f = do
  c <- BS.readFile f
  return $ encoding c

spaces = skipMany (oneOf " \n\r") --skipSpace
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

encoding :: ByteString -> Encoding
encoding c = case parseOnly encodingArray c of
  Right ss -> Encoding $ Map.fromListWith (flip const) ss
  Left _   -> NullMap

encodingArray :: Parser [(Char,T.Text)]
encodingArray = do
  manyTill anyChar (try $ lookAhead $ string "/Encoding")
  string "/Encoding"
  spaces
  choice [ [] <$ (string "StandardEncoding"
                   >> spaces >> string "def")
         , (skipFor >> spaces 
             *> manyTill specialEncodings
             (try $ string "readonly" <|> string "def"))
         ]
    where
      skipFor = manyTill anyChar (try $ string "for")

specialEncodings :: Parser (Char, T.Text)
specialEncodings = do
  spaces 
  (,) <$> (spaces >> string "dup" >> spaces *> index)
    <*> (spaces >> charName <* spaces)
  where
    index = (chr . read) <$> many1 digit
    charName = T.pack <$> manyTill anyChar (try $ (spaces >> string "put"))

