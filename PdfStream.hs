{-# LANGUAGE OverloadedStrings #-}

module PdfStream 
       ( deflate
       , decompressStream
       , parseCMap
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
deflate st = parseDeflated (trace (show st) st)

decompressStream :: PDFBS -> PDFStream
decompressStream (n,pdfobject) = 
  case parse (BSL.pack <$> 
              (manyTill anyChar (try $ (string "stream" >> oneOf "\n\r")) >> spaces
               *> manyTill anyChar (try $ string "endstream"))) "" pdfobject of
    Left err -> "err"
    Right bs -> decompress bs

parseCMap :: BSL.ByteString -> CMap
parseCMap str = case runParser cmapParser () "" str of
  Left err -> error "Can not parse CMap"
  Right cmap -> cmap
  
cmapParser :: Parser CMap
cmapParser = return []


elems :: PSParser T.Text
elems = choice [ try pdfopTf
               , try pdfopTD
               , try pdfopTd
               , try pdfopTm
               , try pdfopTast
               , try letters
               , try hexletters
               , try bore
               , try array
               , unknowns
               ]

unknowns :: PSParser T.Text
unknowns = do 
  ps <- manyTill anyChar (try $ oneOf "\r\n")
  return ""

bore :: PSParser T.Text
bore = do
  string "TJ"
  spaces
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
  return $ T.concat lets
--  return $ if ay > topPt || ay < bottomPt then "" else lets

hexletters :: PSParser T.Text
hexletters = do
  char '<'
  lets <- manyTill hexletter (try $ char '>')
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
  updateState (\s -> s{curfont= font})
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
  updateState (\s -> s { absolutex = ax+(lx*t1)
                       , absolutey = ay+(ly*t2)
                       })
  return $ desideParagraphBreak (ax+(t1*lx)) (t2*ly) lm

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
      needBreak = t2 > 0
  updateState (\s -> s { absolutex = ax+t1
                       , absolutey = ay+t2
                       })
  return $ if needBreak then "\n\n" else desideParagraphBreak t1 t2 lm

desideParagraphBreak :: Double -> Double -> Double -> T.Text
desideParagraphBreak t1 t2 lm = T.pack $
  (if abs (t1 - lm) < 1.0
   then " "
   else if t2 < -15 then "\n\n" else " ")
  ++
  (if t2 < -15 then "\n\n" else " ")

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
  updateState (\s -> s { linex     = a
                       , liney     = d
                       , absolutex = e
                       , absolutey = f
                       })
  return $ if abs (f - ay) < 2*ly then 
             if e/lx < 1.5*lx then "" else " "
           else "\n\n"

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
                       , absolutex = 70
                       , absolutey = ay-ly
                       })
  return " "

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
  

