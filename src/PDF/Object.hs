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
import Data.Text.Encoding.Error (strictDecode)
import Numeric (readOct, readHex)
import Data.ByteString.Builder (toLazyByteString, word16BE)

import Data.Attoparsec.ByteString.Char8 hiding (take, skipWhile, takeTill)
import Data.Attoparsec.ByteString.Char8 as P (takeWhile1)
import Data.Attoparsec.ByteString as W (skipWhile, word8, notWord8, takeTill, anyWord8)
import Data.Attoparsec.Combinator
import Control.Applicative

import Debug.Trace

import PDF.Definition

spaces = many (comment <|> pdfspaces)
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

-- parse pdf objects

pdfObj :: Parser PDFBS
pdfObj = do
  spaces
  objn <- many1 digit <* (spaces >> oneOf "0123456789" >> string " obj")
  object <- {-# SCC "pdfobj_manyTill" #-}manyTill' anyChar (try $ string "endobj")
  spaces
  many xref
  many startxref
  return $ (read objn, BS.pack object)

pdfspaces = do
  W.word8 0 <|> W.word8 9 <|> W.word8 10 <|> W.word8 12 <|> W.word8 13 <|> W.word8 32
  return ""

parsePDFObj :: PDFBS -> PDFObj
parsePDFObj (n,pdfobject) = case parseOnly (spaces >> many1 (try pdfobj <|> try objother)) pdfobject of
  Left  err -> (n,[PdfNull])
  Right obj -> (n,obj)

comment = do
  W.word8 37 >> W.notWord8 37 >> W.takeTill (\w-> w == 13 || w == 10)
  return " "

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
pdfname = PdfName . BS.unpack <$> (BS.append <$> string "/" <*> (BS.pack <$> manyTill anyChar (try $ lookAhead $ oneOf "><][)( \n\r/"))) <* spaces

pdfletters :: Parser Obj
pdfletters = PdfText <$> parsePdfLetters

parsePdfLetters :: Parser String
parsePdfLetters = concat <$> (char '(' *>
                               manyTill (choice [ try pdfutf
                                                , try pdfoctutf
                                                , pdfletter])
                               (try $ char ')'))
  where pdfletter =
          choice [ "(" <$ try (string "\\(")
                 , ")" <$ try (string "\\)")
                 , "\\" <$ try (string "\\\\")
                 , "\n" <$ try (string "\\n")
                 , "\r" <$ try (string "\\r")
                 , "\t" <$ try (string "\\t")
                 , "\b" <$ try (string "\\b")
                 , "\f" <$ try (string "\\f")
                 , octal <$> try (char '\\' *> count 3 (oneOf "01234567"))
                 , octal <$> try (char '\\' *> count 2 (oneOf "01234567"))
                 , octal . (:[]) <$> try (char '\\' *> oneOf "01234567")
                 , return <$> try (noneOf "\\")
                 , "" <$ string "\\"
                 ]

        octal = return . chr . fst . head . readOct
        
        pdfutf :: Parser String
        pdfutf = do 
          str <- string "\254\255" *> manyTill pdfletter (lookAhead $ string ")")
          return $ utf16be $ concat str
        
        pdfoctutf :: Parser String
        pdfoctutf = do
          str <- string "\\376\\377" *> manyTill pdfletter (lookAhead $ string ")")
          return $ utf16be $ concat str
        
utf16be = T.unpack . decodeUtf16BEWith strictDecode . BS.pack

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
pdfobj = choice [ try rrefs
                , try pdfname
                , try pdfnumber
                , try pdfhex <* spaces -- Hexadecimal String
                , try pdfbool <* spaces
                , try pdfnull <* spaces
                , try pdfarray <* spaces
                , try pdfdictionary <* spaces
                , try pdfstream <* spaces
                , pdfletters <* spaces -- Literal String
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
objother = ObjOther . show <$> (W.takeTill (\w-> w == 10 || w == 13) <* (W.word8 10 <|> W.word8 13))

parseRefsArray :: [Obj] -> [Int]
parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
parseRefsArray (x:y)  = (parseRefsArray y)
parseRefsArray [] = []


