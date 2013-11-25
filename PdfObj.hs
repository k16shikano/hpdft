{-# LANGUAGE OverloadedStrings #-}

module PdfObj 
       ( parseTrailer
       , rootRef
       , getRefs
       , resourcesFont
       , grubFontDiff
       , contentsStream
       , pagesKids  
       , pages
       , findDictOfType
       , findObjThroughDict
       , findObjsByRef
       , parsePDFObj
       , getObjs 
       , pdfObj
       ) where

import Data.Char (chr)
import Data.List (find)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import Text.Parsec.ByteString
import Codec.Compression.Zlib (decompress)

import Debug.Trace

import Pdf
import PdfStream


-- parse pdf objects

getObjs :: BS.ByteString -> [PDFBS]
getObjs contents = case parse (many1 pdfObj) "" contents of
  Left  err -> []
  Right rlt -> rlt

pdfObj :: Parser PDFBS
pdfObj = do
  many $ comment <|> oneOf "\r\n"
  objn <- many1 digit <* string " 0 obj"
  object <- manyTill anyChar (try $ string "endobj")
  spaces
  return $ (read objn, BS.pack object)

parsePDFObj :: PDFBS -> PDFObj
parsePDFObj (n,pdfobject) = case parse (spaces >> many1 (pdfobj <|> objother)) "" pdfobject of
  Left  err -> (n,[PdfNull])
  Right obj -> (n,obj)

comment :: Parser Char
comment = do
  char '%'
  manyTill anyChar $ oneOf "\r\n"
  return ' '

stream :: Parser PDFStream
stream = do
  string "stream"
  spaces
  stm <- BSL.pack <$> manyTill anyChar (try $ string "endstream")
  return stm

pdfdictionary :: Parser Obj
pdfdictionary = PdfDict <$> (string "<<" >> spaces *> manyTill dictEntry (try $ spaces >> string ">>"))

dictEntry :: Parser (Obj, Obj)
dictEntry = (,) <$> pdfname <*> pdfobj

pdfarray :: Parser Obj
pdfarray = PdfArray <$> (string "[" >> spaces *> manyTill pdfobj (try $ spaces >> string "]"))

pdfname :: Parser Obj
pdfname = PdfName <$> ((++) <$> string "/" <*> manyTill anyChar (try $ lookAhead $ oneOf "><][)( \n\r/")) <* spaces

pdfletters :: Parser Obj
pdfletters = PdfText <$> (char '(' *> manyTill pdfletter (try $ char ')'))
  where pdfletter  =  (char '\\' >> oneOf "\\()") <|> noneOf "\\"

pdfstream :: Parser Obj
pdfstream = PdfStream <$> stream

pdfnumber :: Parser Obj
pdfnumber = PdfNumber <$> pdfdigit
  where pdfdigit = do 
          sign <- many $ char '-'
          num <- ((++) <$> (("0"++) <$> string ".") <*> many1 digit)
                 <|>
                 ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
          spaces        
          return $ read $ sign ++ num

pdfhex :: Parser Obj
pdfhex = PdfHex <$> hex  
  where hex = do
          char '<'
          lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
          return $ lets

pdfbool :: Parser Obj
pdfbool = PdfBool <$> (True <$ string "true"
                       <|> 
                       False <$ string "false")

pdfnull :: Parser Obj
pdfnull = PdfNull <$ string "null"

pdfobj :: Parser Obj
pdfobj = choice [ try rrefs <* spaces
                , try pdfname <* spaces, try pdfnumber <* spaces, try pdfhex <* spaces
                , try pdfbool <* spaces, try pdfnull <* spaces
                , try pdfarray <* spaces, try pdfdictionary <* spaces, try pdfstream <* spaces
                , pdfletters <* spaces]

rrefs :: Parser Obj
rrefs = do  
  objnum <- many1 digit
  spaces
  string "0 R"
  spaces
  return $ ObjRef (read objnum)

objother :: Parser Obj
objother = ObjOther <$> (manyTill anyChar space)


-- find objects

findObjsByRef :: Int -> [PDFObj] -> Maybe [Obj]
findObjsByRef x pdfobjs = case find (isRefObj (Just x)) pdfobjs of
  Just (_,objs) -> Just objs
  Nothing -> Nothing
  where
    isRefObj (Just x) (y, objs) = if x==y then True else False
    isRefObj _ _ = False

findObjThroughDict :: Int -> String -> [PDFObj] -> Maybe Obj
findObjThroughDict ref name objs = case findObjsByRef ref objs of 
  Just os -> case find isDict os of
    Just (PdfDict d) -> case find isName d of
      Just (_, o) -> Just o
      otherwise -> Nothing
    otherwise -> Nothing
  otherwise -> Nothing
  where isName (PdfName n, _) = if name == n then True else False
        isName _              = False

findDictOfType :: String -> [Obj] -> Maybe Dict
findDictOfType typename objs = case find isDict objs of
  Just (PdfDict d) -> if isType d then Just d else Nothing 
  Nothing          -> Nothing
  where 
    isType dict = (PdfName "/Type",PdfName typename) `elem` dict
 
isDict :: Obj -> Bool
isDict (PdfDict d) = True
isDict _           = False

pages :: Dict -> Maybe Int
pages dict = case find isPagesRef dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing
  where
    isPagesRef (PdfName "/Pages", ObjRef x) = True
    isPagesRef (_,_)                        = False
    
pagesKids :: Dict -> Maybe [Int]
pagesKids dict = case find isKidsRefs dict of
  Just (_, PdfArray arr) -> Just (parseRefsArray arr)
  Nothing                -> Nothing
  where 
    parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
    parseRefsArray [] = []
    isKidsRefs (PdfName "/Kids", PdfArray x) = True
    isKidsRefs (_,_)                         = False

contentsStream :: Dict -> PSR -> [PDFObj] -> PDFStream
contentsStream dict st objs = case find content dict of
  Just (PdfName "/Contents", ObjRef x) -> case findObjsByRef x objs of
    Just contobjs -> case find isStream contobjs of
      Just (PdfStream strm) -> deflate (st {fontmaps=fontdict}) strm
      Nothing               -> ""
  Nothing -> ""
  where
    isStream (PdfStream s) = True
    isStream _             = False
    content (PdfName "/Contents", ObjRef x) = True
    content _                               = False
    fontdict = resourcesFont dict objs


-- make fontmap from /Resources

resourcesFont :: Dict -> [PDFObj] -> [(String, FontMap)]
resourcesFont dict objs = case find resources dict of
  Just (PdfName "/Resources", ObjRef x) -> case findObjThroughDict x "/Font" objs of
    Just (PdfDict d) -> fonts d objs
    otherwise -> []
  otherwise -> []
  where
    resources (PdfName "/Resources", ObjRef x) = True
    resources _                                = False

grubFontDiff :: Int -> [PDFObj] -> [(String, FontMap)]
grubFontDiff ref objs = case findObjThroughDict ref "/Resources" objs of
  Just (ObjRef rref) -> case findObjThroughDict rref "/Font" objs of
    Just (PdfDict d) -> fonts d objs
    otherwise -> []
  otherwise -> []

fonts :: Dict -> [PDFObj] -> [(String, FontMap)]
fonts dict objs = map pairwise dict
  where 
    pairwise (PdfName n, ObjRef r) = (n, findFontMap r objs)
    pairwise x = ("",[])

findFontMap :: Int -> [PDFObj] -> FontMap
findFontMap x objs = case findObjThroughDict x "/Encoding" objs of
  Just (ObjRef ref) -> case findObjThroughDict ref "/Differences" objs of
    Just (PdfArray arr) -> charMap arr
    otherwise -> []
  otherwise -> []

charMap :: [Obj] -> FontMap
charMap objs = fontmap objs 0
  where fontmap (PdfNumber x : PdfName n : xs) i = 
          if i < truncate x then 
            (chr $ truncate x, n) : (fontmap xs $ incr x)
          else 
            (chr $ i, n) : (fontmap xs $ i+1)
        fontmap (PdfName n : xs) i               = (chr i, n) : (fontmap xs $ i+1)
        fontmap [] i                             = []
        incr x = (truncate x) + 1


-- find root ref from Trailer

parseTrailer :: BS.ByteString -> Maybe Dict
parseTrailer bs = case parse trailer "" bs of
  Left  err -> Nothing
  Right rlt -> case parse (spaces >> pdfdictionary <* spaces) "" rlt of
    Left  err  -> Nothing
    Right (PdfDict dict) -> Just dict
    Right other -> Nothing

trailer :: Parser BS.ByteString
trailer = do
  manyTill anyChar (try $ string "trailer")
  t <- manyTill anyChar (try $ string "startxref")
  return $ BS.pack t

rootRef :: BS.ByteString -> Maybe Int
rootRef bs = getRefs isRootRef $ parseTrailer bs
  where isRootRef (PdfName "/Root", ObjRef x) = True
        isRootRef (_,_) = False

getRefs :: ((Obj,Obj) -> Bool) -> Maybe Dict -> Maybe Int
getRefs pred (Just objs) = case find pred objs of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing
getRefs _ _ = Nothing

