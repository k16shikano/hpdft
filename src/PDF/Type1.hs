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

import Debug.Trace

import PDF.Definition

test f = do
  c <- BS.readFile f
  return $ encoding c

spaces = skipMany (oneOf " \n\r") --skipSpace
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

encoding :: ByteString -> Encoding
encoding c = case parseOnly encodingArray c of
  Right ss -> Encoding ss
  Left e -> error "Can not find /Encoding in the Type1 Font"

encodingArray :: Parser [(Char,String)]
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

specialEncodings :: Parser (Char, String)
specialEncodings = do
  spaces 
  (,) <$> (spaces >> string "dup" >> spaces *> index)
    <*> (spaces >> charName <* spaces)
  where
    index = (chr . read) <$> many1 digit
    charName = manyTill anyChar (try $ (spaces >> string "put"))

