{-# LANGUAGE OverloadedStrings #-}

module PDF.Cmap
       ( parseCMap
       ) where

import Data.Char (chr)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Numeric (readHex)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import Text.Parsec.ByteString.Lazy
import Codec.Compression.Zlib (decompress) 

import PDF.Definition

parseCMap :: BSL.ByteString -> CMap
parseCMap str
  | BSL.null str = []
  | otherwise =
      case runParser (skipHeader >>
                      concat <$>
                      manyTill
                       (choice
                       [ try bfchar
                       , try $ concat <$> bfrange
                       ])
                       (try $ string "endcmap"))
                     () "" str of
        Left _  -> []
        Right cmap -> mkUniq cmap

  where
    mkUniq = reverse

readHexSafe :: String -> Maybe Int
readHexSafe s = case readHex s of
  [(n, "")] -> Just n
  _         -> Nothing

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
  return $ concatMap maybeToList ms
    where
      maybeToList Nothing = []
      maybeToList (Just x) = [x]
      toCmap cid ucs =
        case (readHexSafe cid, readHexSafe (take 4 ucs)) of
          (Just c, Just u) -> Just (c, [chr u])
          _                -> Nothing

bfrange :: Parser [CMap]
bfrange = do
  d <- many1 digit
  spaces 
  string "beginbfrange"
  spaces
  ms <- many (toCmap
              <$> (getRange <$> hexletters <*> hexletters)
              <*> ((mkStrList d . lines) <$> (try hexletters <|> hexletterArray)))
  spaces
  string "endbfrange"
  spaces
  return ms
    where 
      gethex = readHexSafe
      getRange cid cid' =
        case (gethex cid, gethex cid') of
          (Just a, Just b) -> [a .. b]
          _                -> []
      mkStrList d src = if length src == 1
                        then case src of
                               (s:_) -> case gethex s of
                                          Just n -> [n .. ]
                                          Nothing -> []
                               [] -> []
                        else mapMaybe gethex src
      toCmap range ucs = zip range (map ((:[]).chr) ucs)


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

