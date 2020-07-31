{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Object
Description : Function to parse objects in a PDF file
Copyright   : (c) Keiichiro Shikano, 2016
License     : MIT
Maintainer  : k16.shikano@gmail.com

Functions to parsea and show objects in a PDF file. 
It provides a basic way to find information from a PDF file.
-}

module PDF.Object
  ( parsePdfLetters
  , parsePDFObj
  , parseRefsArray
  , pdfObj
  , pdfletters
  , pdfarray
  , pdfdictionary
  , xref
  ,
  ) where

import Data.Char (chr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16BEWith)
import Data.Text.Encoding.Error (lenientDecode)
import Numeric (readOct, readHex)
import Data.ByteString.Builder (toLazyByteString, word16BE)

import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Combinator
import Control.Applicative

import Debug.Trace

import PDF.Definition

spaces = skipMany (comment <|> oneOf pdfspaces) --skipSpace
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

-- parse pdf objects

pdfObj :: Parser PDFBS
pdfObj = do
  spaces -- skipMany (comment <|> oneOf pdfspaces)
  objn <- many1 digit <* (spaces >> oneOf "0123456789" >> string " obj")
  object <- manyTill anyChar (try $ string "endobj")
  spaces
  skipMany xref
  skipMany startxref
  return $ (read objn, BS.pack object)

pdfspaces :: [Char]
pdfspaces = map chr [0, 9, 10, 12, 13, 32]

parsePDFObj :: PDFBS -> PDFObj
parsePDFObj (n,pdfobject) = case parseOnly (spaces >> many1 (try pdfobj <|> try objother)) pdfobject of
  Left  err -> (n,[PdfNull])
  Right obj -> (n,obj)

comment :: Parser Char
comment = do
  char '%'
  noneOf "%"
  manyTill anyChar $ oneOf "\r\n"
  return ' '

xref :: Parser String
xref = do
  spaces
  string "xref"
  spaces
  ref <- manyTill anyChar (try $ string "%%EOF")
  spaces
  return ""

startxref :: Parser String
startxref = do
  spaces
  string "startxref"
  spaces
  ref <- manyTill anyChar (try $ string "%%EOF")
  spaces
  return ""
  
stream :: Parser PDFStream
stream = do
  string "stream"
  spaces
  stm <- BSL.pack <$> manyTill anyChar (try $ string "endstream")
  return stm

pdfdictionary :: Parser Obj
pdfdictionary = PdfDict <$> (spaces >> string "<<" >> spaces *> manyTill dictEntry (try $ spaces >> string ">>"))

dictEntry :: Parser (Obj, Obj)
dictEntry = (,) <$> pdfname <*> pdfobj

pdfarray :: Parser Obj
pdfarray = PdfArray <$> (string "[" >> spaces *> manyTill pdfobj (try $ spaces >> string "]"))

pdfname :: Parser Obj
pdfname = PdfName . BS.unpack <$> (BS.append <$> string "/" <*> (BS.pack <$> (manyTill anyChar (try $ lookAhead $ oneOf "><][)( \n\r/")))) <* spaces

pdfletters :: Parser Obj
pdfletters = PdfText <$> parsePdfLetters

parsePdfLetters :: Parser String
parsePdfLetters = (concat <$> (char '(' *> manyTill (choice [try pdfutf, try pdfoctutf, pdfletter]) (try $ char ')')))
  where pdfletter = do
          str <- choice [ return <$> try (char '\\' >> oneOf "\\()")
                        , "\n" <$ try (string "\n")
                        , "\r" <$ try (string "\r")
                        , "\t" <$ try (string "\t")
                        , "\b" <$ try (string "\b")
                        , "\f" <$ try (string "\f")
                        , (++) <$> ("(" <$ char '(') <*> ((++")") . concat <$> manyTill pdfletter (try $ char ')'))
                        , return <$> (noneOf "\\")
                        ]
          return $ str
        pdfutf :: Parser String
        pdfutf = do 
          str <- string "\254\255" *> manyTill anyChar (lookAhead $ string ")")
          return $ utf16be str
        
        pdfoctutf :: Parser String
        pdfoctutf = do
          string "\\376\\377" 
          octstr <- manyTill (choice [ try (return . chr . fst . head . readOct <$> (char '\\' *> count 3 (oneOf "01234567")))
                                     , try ("\92" <$ string "\\\\")
                                     , return <$> noneOf "\\"
                                     ])
                    (lookAhead $ string ")")
          return $ utf16be $ concat octstr

        octToString [] = "????"
        octToString [(o,_)] = [chr o]

utf16be = T.unpack . decodeUtf16BEWith lenientDecode . BS.pack

pdfstream :: Parser Obj
pdfstream = PdfStream <$> stream

pdfnumber :: Parser Obj
pdfnumber = PdfNumber <$> pdfdigit
  where pdfdigit = do 
          sign <- many $ char '-'
          num <- ((++) <$> (("0"++) . BS.unpack <$> string ".") <*> (many1 digit))
                 <|>
                 ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
          spaces
          return $ read $ sign ++ num

pdfhex :: Parser Obj
pdfhex = PdfHex <$> hex
  where hex = do
          char '<'
          lets <- BS.pack <$> manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
          case parseOnly ((try $ string "feff" <|> string "FEFF") *> (many1 (oneOf "0123456789abcdefABCDEF"))) lets of
            Right s -> return $ pdfhexletter $ BS.pack s
            Left e -> return . BS.unpack $ lets

pdfhexletter s = case parseOnly (concat <$> many1 pdfhexutf16be) s of
  Right t -> utf16be t
  Left e -> BS.unpack s

pdfhexutf16be :: Parser String
pdfhexutf16be = do
  c <- count 4 $ oneOf "0123456789ABCDEFabcdef"
  let b = BSL.unpack . toLazyByteString . word16BE $ fst . head . readHex $ c
  return $ b

pdfbool :: Parser Obj
pdfbool = PdfBool <$> (True <$ string "true"
                       <|> 
                       False <$ string "false")

pdfnull :: Parser Obj
pdfnull = PdfNull <$ string "null"

pdfobj :: Parser Obj
pdfobj = choice [ try rrefs <* spaces
                , try pdfname <* spaces
                , try pdfnumber <* spaces
                , try pdfhex <* spaces
                , try pdfbool <* spaces
                , try pdfnull <* spaces
                , try pdfarray <* spaces
                , try pdfdictionary <* spaces
                , {-# SCC pdfstream #-} try pdfstream <* spaces
                , pdfletters <* spaces
                ]

rrefs :: Parser Obj
rrefs = do  
  objnum <- many1 digit
  spaces
  oneOf "0123456789"
  spaces
  string "R"
  spaces
  return $ ObjRef (read objnum)

objother :: Parser Obj
objother = ObjOther <$> (manyTill anyChar space)

parseRefsArray :: [Obj] -> [Int]
parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
parseRefsArray (x:y)  = (parseRefsArray y)
parseRefsArray [] = []


