{-# LANGUAGE OverloadedStrings #-}

module PdfStream 
       ( deflate
       , decompressStream
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

import Pdf
import PdfCharDict (pdfchardict)

type PSParser a = GenParser Char PSR a

parsePage p st = runParser p st ""

parseDeflated :: PSR -> BSL.ByteString -> PDFStream
parseDeflated psr pdfstream = case parsePage (T.concat <$> many (elems <|> skipOther)) psr pdfstream of
  Left  err -> error "Nothing to be parsed"
  Right str -> BSL.pack $ BS.unpack $ encodeUtf8 str

deflate :: PSR -> PDFStream -> PDFStream
deflate = parseDeflated

decompressStream :: PDFBS -> PDFStream
decompressStream (n,pdfobject) = 
  case parse (BSL.pack <$> 
              (manyTill anyChar (try $ (string "stream" >> oneOf "\n\r")) >> spaces
               *> manyTill anyChar (try $ string "endstream"))) "" pdfobject of
    Left err -> "err"
    Right bs -> decompress bs

elems :: PSParser T.Text
elems = choice [ try pdfopBT 
               , try pdfopTf
               , try pdfopTD
               , try pdfopTd
               , try pdfopTm
               , try pdfopTj
               , try pdfopTast
               , try letters <* spaces
               , try hexletters <* spaces
               , try array <* spaces
               , unknowns
               ]

pdfopBT :: PSParser T.Text
pdfopBT = do
  string "BT"
  spaces
  t <- manyTill elems (try $ string "ET")
  spaces
  return $ T.concat t

pdfopTj :: PSParser T.Text
pdfopTj = do
  t <- manyTill (letters <|> hexletters) (try $ (string "Tj" <|> string "TJ"))
  spaces
  return $ T.concat t
  
unknowns :: PSParser T.Text
unknowns = do 
  ps <- manyTill anyChar (try $ oneOf "\r\n")
  return ""

skipOther :: PSParser T.Text
skipOther = do
  a <- manyTill anyChar (try $ oneOf "\r\n")
  return $ ""

array :: PSParser T.Text
array = do
  char '['
  str <- (manyTill (letters <|> hexletters <|> kern) (try $ char ']'))
  return $ T.concat str

letters :: PSParser T.Text
letters = do
  char '('
  lets <- manyTill psletter (try $ char ')')
  spaces
  return $ T.concat lets

hexletters :: PSParser T.Text
hexletters = do
  char '<'
  lets <- manyTill hexletter (try $ char '>')
  spaces
  return $ T.concat lets

adobeOneSix :: Int -> T.Text
adobeOneSix  a  = T.pack (show a)

toUcs :: CMap -> Int -> T.Text
toUcs map h = case lookup h map of
  Just ucs -> T.pack ucs
  Nothing -> adobeOneSix h

hexletter :: PSParser T.Text
hexletter = do
  st <- getState
  let cmap = case lookup (curfont st) (cmaps st) of
        Just m -> m
        Nothing -> []
  (hexToString cmap . readHex) <$> (count 4 $ oneOf "0123456789ABCDEFabcdef")
  where hexToString map [] = "????"
        hexToString map [(h,_)] = toUcs map h

psletter :: PSParser T.Text
psletter = do
  st <- getState
  let fontmap = case lookup (curfont st) (fontmaps st) of
        Just m -> m
        Nothing -> []
  c <- try (char '\\' >> oneOf "\\()")
       <|>
       try (octToString . readOct <$> (char '\\' >> (count 3 $ oneOf "01234567")))
       <|>
       noneOf "\\"
  return $ replaceWithDiff fontmap c
    where replaceWithDiff m c' = case lookup c' m of
            Just s -> replaceWithCharDict s
            Nothing -> T.pack [c']
          replaceWithCharDict s = case lookup s pdfchardict of
            Just cs -> cs
            Nothing -> T.pack s
          octToString [] = '?'
          octToString [(o,_)] = chr o

kern :: PSParser T.Text
kern = do
  t <- digitParam
  return $ if t < -60.0 then " " else ""

pdfopTf :: PSParser T.Text
pdfopTf = do
  font <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  t <- digitParam
  spaces
  string "Tf"
  spaces
  st <- getState
  let ff = fontfactor st
  updateState (\s -> s{ curfont = font
                      , fontfactor = ff*t})
  return ""

pdfopTD :: PSParser T.Text
pdfopTD = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "TD"
  spaces
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      needBreak = abs t2 > 0 && abs (ly - t2) > 0
  updateState (\s -> s { absolutex = ax + t1
                       , absolutey = ay + (if needBreak then 2*t2 else 0)
                       , linex = if abs t1 > 0 then t1 else lx
                       , liney = if abs t2 > 0 then 2*t2 else ly
                       })
  return $ if needBreak 
           then T.concat ["\n", (desideParagraphBreak t1 t2 lx ly lm ff)] 
           else ""

pdfopTd :: PSParser T.Text
pdfopTd = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "Td"
  spaces
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      needBreak = abs t2 > 0 && abs (ly - t2) > 0
  updateState (\s -> s { absolutex = ax + t1
                       , absolutey = ay + (if needBreak then t2 else 0)
                       , linex = if abs t1 > 0 then t1 else lx
                       , liney = if abs t2 > 0 then t2 else ly
                       })
  return $ if needBreak 
           then T.concat ["\n", (desideParagraphBreak t1 t2 lx ly lm ff)] 
           else ""

desideParagraphBreak :: Double -> Double -> Double -> Double -> Double -> Double 
                     -> T.Text
desideParagraphBreak t1 t2 lx ly lm ff = T.pack $
  (if abs ly > abs ff || abs t2 > abs ff || (t1 - lm) > 0.5
   then "\n"
   else "")

pdfopTm :: PSParser T.Text
pdfopTm = do
  a <- digitParam
  spaces
  b <- digitParam
  spaces
  c <- digitParam
  spaces
  d <- digitParam
  spaces
  e <- digitParam
  spaces
  f <- digitParam
  spaces
  string "Tm"
  spaces
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      needBreak = abs d*f > 0 && ly >= 0
  updateState (\s -> s { linex     = lx
                       , liney     = ly
                       , absolutex = a*e
                       , absolutey = d*f
                       , fontfactor = a*e*d*f*ff
                       })
  return $ if needBreak 
           then T.concat ["\n", desideParagraphBreak (d*f) (d*f) lx ly lm ff]
           else if a*e > lx then " " else ""

pdfopTast :: PSParser T.Text
pdfopTast = do
  string "T*"
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
  updateState (\s -> s { linex     = lx
                       , liney     = ly
                       , absolutex = ax
                       , absolutey = ay
                       })
  return "\n"

digitParam :: PSParser Double
digitParam = do 
  sign <- many $ char '-'
  num <- ((++) <$> (("0"++) <$> (string ".")) <*> many1 digit)
         <|>
         ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
  return $ read $ sign ++ num

hexParam :: Parser T.Text
hexParam = do
  char '<'
  lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
  return $ T.pack lets
