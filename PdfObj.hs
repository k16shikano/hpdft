{-# LANGUAGE OverloadedStrings #-}

module PdfObj 
       ( parseTrailer
       , rootRef
       , getRefs
       , getPdfObjByRef
       , getPDFBSFile
       , getPDFObjFile
       , getRootRefFile
       , contentsStream
       , rawContentsStream
       , pagesKids  
       , pages
       , findDictOfType
       , findObjThroughDict
       , findObjsByRef
       , parsePDFObj
       , getObjs 
       , pdfObj
       , getXref
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


-- IO utilities

getPDFBSFile :: String -> IO [PDFBS]
getPDFBSFile f = do
  c <- BS.readFile f
  let bs = getObjs c
  return bs

getPDFObjFile :: String -> IO [PDFObj]
getPDFObjFile f = do
  c <- BS.readFile f
  let obj = map parsePDFObj $ getObjs c
  return obj

getPdfObjByRef :: Int -> IO [PDFObj] -> IO [Obj]
getPdfObjByRef ref ioobjs = do
  objs <- ioobjs
  case findObjsByRef ref objs of
    Just os -> return $ os
    Nothing -> error $ "No Object with Ref " ++ show ref

getRootRefFile :: String -> IO (Maybe Int)
getRootRefFile f = do
  c <- BS.readFile f
  return $ rootRef c


-- parse pdf objects

getObjs :: BS.ByteString -> [PDFBS]
getObjs contents = case parse (many1 pdfObj) "" contents of
  Left  err -> []
  Right rlt -> rlt

getXref :: BS.ByteString -> String
getXref contents = case parse (xref) "" contents of
  Left  err -> []
  Right rlt -> rlt

pdfObj :: Parser PDFBS
pdfObj = do
  skipMany (comment <|> oneOf "\r\n")
  objn <- many1 digit <* string " 0 obj"
  object <- manyTill anyChar (try $ string "endobj")
  spaces
  skipMany xref
  return $ (read objn, BS.pack object)

parsePDFObj :: PDFBS -> PDFObj
parsePDFObj (n,pdfobject) = case parse (spaces >> many1 (pdfobj <|> objother)) "" pdfobject of
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

findObjThroughDictByRef :: Int -> String -> [PDFObj] -> Maybe Obj
findObjThroughDictByRef ref name objs = case findDictByRef ref objs of 
  Just d -> findObjThroughDict d name
  Nothing -> Nothing
  
findObjThroughDict :: Dict -> String -> Maybe Obj
findObjThroughDict d name = case find isName d of
  Just (_, o) -> Just o
  otherwise -> Nothing
  where isName (PdfName n, _) = if name == n then True else False
        isName _              = False

findDictByRef :: Int -> [PDFObj] -> Maybe Dict
findDictByRef ref objs = case findObjsByRef ref objs of
  Just os -> findDict os
  Nothing -> Nothing

findDictOfType :: String -> [Obj] -> Maybe Dict
findDictOfType typename objs = case findDict objs of
  Just d  -> if isType d then Just d else Nothing 
  Nothing -> Nothing
  where 
    isType dict = (PdfName "/Type",PdfName typename) `elem` dict
 
findDict :: [Obj] -> Maybe Dict
findDict objs = case find isDict objs of
  Just (PdfDict d) -> Just d
  otherwise -> Nothing
  where 
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
    isKidsRefs (PdfName "/Kids", PdfArray x) = True
    isKidsRefs (_,_)                         = False

contentsStream :: Dict -> PSR -> [PDFObj] -> PDFStream
contentsStream dict st objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> BSL.concat $ map (parsedContentStreamByRef dict st objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> parsedContentStreamByRef dict st objs x
  Nothing                                  -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False

rawContentsStream :: Dict -> [PDFObj] -> PDFStream
rawContentsStream dict objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> BSL.concat $ map (rawStreamByRef objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> rawStreamByRef objs (trace (show x) x)
  Nothing                                  -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False

parsedContentStreamByRef :: Dict -> PSR -> [PDFObj] -> Int -> PDFStream
parsedContentStreamByRef dict st objs ref = deflate (st {fontmaps=fontdict, cmaps=cmap}) $ rawStreamByRef objs ref
  where fontdict = findFontMap dict objs
        cmap = findCMap dict objs

rawStreamByRef objs x = case findObjsByRef x objs of
  Just sobjs -> case find isStream sobjs of
    Just (PdfStream strm) -> decompress strm
    Nothing               -> error "No stream to be shown"
  Nothing -> error "No stream to be shown"
  where
    isStream (PdfStream s) = True
    isStream _             = False

parseRefsArray :: [Obj] -> [Int]
parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
parseRefsArray [] = []



-- make fontmap from page's /Resources (see 3.7.2 of PDF Ref.)

findFontMap d os = encoding (getFontObjs d os) os

encoding :: Dict -> [PDFObj] -> [(String, FontMap)]
encoding dict objs = map pairwise dict
  where 
    pairwise (PdfName n, ObjRef r) = (n, fontMap r objs)
    pairwise x = ("",[])

findResourcesDict :: Dict -> [PDFObj] -> Maybe Dict
findResourcesDict dict objs = case find resources dict of
  Just (_, ObjRef x)  -> findDictByRef x objs
  Just (_, PdfDict d) -> Just d
  otherwise -> error (show dict)
  where
    resources (PdfName "/Resources", _) = True
    resources _                         = False

getFontObjs :: Dict -> [PDFObj] -> Dict
getFontObjs dict objs = case findResourcesDict dict objs of
  Just d -> case findObjThroughDict d "/Font" of
    Just (PdfDict d) -> d
    otherwise -> []
  Nothing -> []


-- Needs rewrite!
fontMap :: Int -> [PDFObj] -> FontMap
fontMap x objs = case findObjThroughDictByRef x "/Encoding" objs of
  Just (ObjRef ref) -> case findObjThroughDictByRef ref "/Differences" objs of
    Just (PdfArray arr) -> charMap arr
    otherwise -> []
  Just (PdfName "/StandardEncoding") -> (trace "standard enc." [])
  Just (PdfName "/MacRomanEncoding") -> (trace "mac roman enc." [])
  Just (PdfName "/MacExpertEncoding") -> (trace "mac expert enc." [])
  Just (PdfName "/WinAnsiEncoding") -> (trace "win ansi enc." [])
  otherwise -> case findObjThroughDictByRef x "/FontDescriptor" objs of
    Just (ObjRef ref) -> case findObjThroughDictByRef ref "/CharSet" objs of
      Just (PdfText str) -> []
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

findCMap d os = cMap (getFontObjs d os) os

cMap :: Dict -> [PDFObj] -> [(String, CMap)]
cMap dict objs = map pairwise dict
  where
    pairwise (PdfName n, ObjRef r) = (n, toUnicode r objs)
    pairwise x = ("", [])

toUnicode :: Int -> [PDFObj] -> CMap
toUnicode x objs = case findObjThroughDictByRef x "/Encoding" objs of
  Just (PdfName "/Identity-H") -> case findObjThroughDictByRef x "/ToUnicode" objs of
    Just (ObjRef ref) -> (trace (show $ rawStreamByRef objs ref) (parseCMap $ rawStreamByRef objs ref))
    otherwise -> []
  otherwise -> []


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

