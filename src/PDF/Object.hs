{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Object
Description : Function to parse objects in a PDF file
Copyright   : (c) Keiichiro Shikano, 2016
License     : MIT
Maintainer  : k16.shikano@gmail.com

Functions to parsea and show objects in a PDF file. 
It provides a basic way to get information from a PDF file.
-}

module PDF.Object
       ( parseTrailer
       , findTrailer
       , rootRef
       , contentsStream
       , rawContentsStream
       , rawStreamByRef
       , rawStream
       , contentsColorSpace
       , toUnicode
       , pagesKids
       , pages
       , findDict
       , findDictByRef
       , findDictOfType
       , findObjThroughDict
       , findObjThroughDictByRef
       , findObjsByRef
       , parsePDFObj
       , parseRefsArray
       , parsePdfLetters
       , getObjs
       , pdfObj
       , getRefs
       , getXref
       , expandObjStm
       ) where

import Data.Char (chr)
import Data.List (find)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16BEWith)
import Data.Text.Encoding.Error (lenientDecode)
import Numeric (readOct, readHex)
import Data.ByteString.Builder (toLazyByteString, word16BE)

import Data.Attoparsec.ByteString hiding (inClass, notInClass, satisfy, take)
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Combinator
import Control.Applicative
import Codec.Compression.Zlib (decompress)

import Debug.Trace

import PDF.Definition
import PDF.ContentStream
import PDF.Cmap

spaces = skipSpace
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

-- parse pdf objects

getObjs :: BS.ByteString -> [PDFBS]
getObjs contents = case parseOnly (many1 pdfObj) contents of
  Left  err -> []
  Right rlt -> rlt

getXref :: BS.ByteString -> String
getXref contents = case parseOnly (xref) contents of
  Left  err -> []
  Right rlt -> rlt

pdfObj :: Parser PDFBS
pdfObj = do
  skipMany (comment <|> oneOf "\r\n")
  objn <- many1 digit <* (spaces >> oneOf "0123456789" >> string " obj")
  object <- manyTill anyChar (try $ string "endobj")
  spaces
  skipMany xref
  skipMany startxref
  return $ (read objn, BS.pack object)

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
  Just (PdfName "/Contents", PdfArray arr) -> parseContentStream dict st objs $ BSL.concat $ map (rawStreamByRef objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> parsedContentStreamByRef dict st objs x
  Nothing                                  -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False

rawContentsStream :: Dict -> [PDFObj] -> PDFStream
rawContentsStream dict objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> BSL.concat $ map (rawStreamByRef objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> rawStreamByRef objs x
  Nothing                                  -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False

parsedContentStreamByRef :: Dict -> PSR -> [PDFObj] -> Int -> PDFStream
parsedContentStreamByRef dict st objs ref = 
  deflate (st {fontmaps=fontdict, cmaps=cmap}) $ rawStreamByRef objs ref
  where fontdict = findFontMap dict objs
        cmap = findCMap dict objs

parseContentStream :: Dict -> PSR -> [PDFObj] -> BSL.ByteString -> PDFStream
parseContentStream dict st objs s = 
  deflate (st {fontmaps=fontdict, cmaps=cmap}) s
  where fontdict = findFontMap dict objs
        cmap = findCMap dict objs

rawStreamByRef :: [PDFObj] -> Int -> BSL.ByteString
rawStreamByRef objs x = case findObjsByRef x objs of
  Just objs -> rawStream objs
  Nothing  -> error "No object with stream to be shown"

rawStream :: [Obj] -> BSL.ByteString
rawStream objs = case find isStream objs of
  Just (PdfStream strm) -> streamFilter strm
  Nothing               -> error $ (show objs) ++ "\n  No stream to be shown"
  where
    isStream (PdfStream s) = True
    isStream _             = False

    streamFilter = case findDict objs of
                     Just d -> case find withFilter d of
                                 Just (PdfName "/Filter", PdfName "/FlateDecode")
                                   -> decompress
                                 Just _ -> id -- need fix
                                 Nothing -> id
                     Nothing -> id
    withFilter (PdfName "/Filter", _) = True
    withFilter _                      = False


parseRefsArray :: [Obj] -> [Int]
parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
parseRefsArray (x:y)  = (parseRefsArray y)
parseRefsArray [] = []

contentsColorSpace :: Dict -> PSR -> [PDFObj] -> [T.Text]
contentsColorSpace dict st objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> concat $ map (parseColorSpace (st {xcolorspaces=xobjcs}) . rawStreamByRef objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> parseColorSpace (st {xcolorspaces=xobjcs}) $ rawStreamByRef objs x
  Nothing                                  -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False
    xobjcs = findXObjectColorSpace dict objs


-- make fontmap from page's /Resources (see 3.7.2 of PDF Ref.)

findFontMap d os = findEncoding (getFontObjs d os) os

findEncoding :: Dict -> [PDFObj] -> [(String, FontMap)]
findEncoding dict objs = map pairwise dict
  where 
    pairwise (PdfName n, ObjRef r) = (n, fontMap r objs)
    pairwise x = ("", NullMap)

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
    otherwise -> trace "no /differences" NullMap
  Just (PdfName "/StandardEncoding") -> NullMap
  Just (PdfName "/MacRomanEncoding") -> NullMap
  Just (PdfName "/MacExpertEncoding") -> NullMap
  Just (PdfName "/WinAnsiEncoding") -> NullMap
  otherwise -> case findObjThroughDictByRef x "/ToUnicode" objs of
    Just (ObjRef ref) -> case findObjThroughDictByRef ref "/CharSet" objs of
      Just (PdfText str) -> WithCharSet str
      otherwise -> WithCharSet ""
    otherwise -> case findObjThroughDictByRef x "/DescendantFonts" objs of -- needs CID to Unicode map
      Just (ObjRef ref) -> case findObjsByRef ref objs of
        Just [(PdfArray ((ObjRef subref):_))] -> case findObjThroughDictByRef subref "/CIDSystemInfo" objs of
          Just (ObjRef inforef) -> case findObjThroughDictByRef inforef "/Registry" objs of
            Just (PdfText "Adobe") -> case findObjThroughDictByRef inforef "/Ordering" objs of
              Just (PdfText "Japan1") -> case findObjThroughDictByRef inforef "/Supplement" objs of
                Just (PdfNumber _) -> CIDmap "Adobe-Japan1"
                _ -> trace (show inforef) defaultCIDMap
              _ -> trace (show inforef) defaultCIDMap
            _ -> trace (show inforef) defaultCIDMap
          _ -> trace (show subref ++ " no /cidsysteminfoy. using Adobe-Japan1...") defaultCIDMap
        _ -> trace (show ref ++ " no array in /descendantfonts. using Adobe-Japan1...") defaultCIDMap
      _ -> trace (show x ++ " no /descendantfonts. using Adobe-Japan1...") defaultCIDMap

  where
    defaultCIDMap = CIDmap "Adobe-Japan1"

charMap :: [Obj] -> FontMap
charMap objs = FontMap $ fontmap objs 0
  where fontmap (PdfNumber x : PdfName n : xs) i = 
          if i < truncate x then 
            (chr $ truncate x, n) : (fontmap xs $ incr x)
          else 
            (chr $ i, n) : (fontmap xs $ i+1)
        fontmap (PdfName n : xs) i = (chr i, n) : (fontmap xs $ i+1)
        fontmap [] i               = []
        incr x = (truncate x) + 1

findCMap d os = cMap (getFontObjs d os) os

cMap :: Dict -> [PDFObj] -> [(String, CMap)]
cMap dict objs = map pairwise dict
  where
    pairwise (PdfName n, ObjRef r) = (n, toUnicode r objs)
    pairwise x = ("", [])

toUnicode :: Int -> [PDFObj] -> CMap
toUnicode x objs = case findObjThroughDictByRef x "/ToUnicode" objs of
  Just (ObjRef ref) -> parseCMap $ rawStreamByRef objs ref
  otherwise -> case findObjThroughDictByRef x "/Encoding" objs of
    Just (PdfName "/Identity-H") -> case findObjThroughDictByRef x "/ToUnicode" objs of
      Just (ObjRef ref) -> parseCMap $ rawStreamByRef objs ref
      otherwise -> []
    otherwise -> []

-- find XObject

findXObjectColorSpace d os = xobjColorSpaceMap (getXObject d os) os

xobjColorSpaceMap dict objs = map pairwise dict
  where
    pairwise (PdfName n, ObjRef r) = xobjColorSpace r objs
    pairwise x = ""

getXObject dict objs = case findResourcesDict dict objs of
  Just d -> case findObjThroughDict d "/XObject" of
    Just (PdfDict d) -> d
    otherwise -> []
  Nothing -> []

xobjColorSpace :: Int -> [PDFObj] -> String
xobjColorSpace x objs = case findObjThroughDictByRef x "/ColorSpace" objs of
  Just (PdfName cs) -> cs
  otherwise -> ""


-- find root ref from Trailer or Cross-Reference Dictionary

parseTrailer :: BS.ByteString -> Maybe Dict
parseTrailer bs = case parseOnly (try trailer <|> xref) bs of
  Left  err -> (trace (show err) Nothing)
  Right rlt -> Just (parseCRDict rlt)
  where trailer :: Parser BS.ByteString
        trailer = do
          manyTill anyChar (try $ string "trailer")
          t <- manyTill anyChar (try $ string "startxref")
          return $ BS.pack t
        xref :: Parser BS.ByteString
        xref = do
          manyTill anyChar (try $ string "startxref" >> spaces >> lookAhead (oneOf "123456789"))
          offset <- many1 digit
          return $ BS.drop (read offset :: Int) bs

parseCRDict :: BS.ByteString -> Dict
parseCRDict rlt = case parseOnly crdict rlt of
  Left  err  -> error $ show (BS.take 100 rlt)
  Right (PdfDict dict) -> dict
  Right other -> error "Could not find Cross-Reference dictionary"
  where crdict :: Parser Obj
        crdict = do 
          spaces
          many (many1 digit >> spaces >> digit >> string " obj" >> spaces)
          d <- pdfdictionary <* spaces
          return d

rootRef :: BS.ByteString -> Maybe Int
rootRef bs = case parseTrailer bs of
  Just dict -> getRefs isRootRef dict
  Nothing   -> rootRefFromCRStream bs

rootRefFromCRStream :: BS.ByteString -> Maybe Int
rootRefFromCRStream bs =
  let offset = (read . BS.unpack . head . drop 1 . reverse . BS.lines $ (trace (show bs) bs)) :: Int
      crstrm = snd . head . getObjs $ BS.drop offset bs
      crdict = parseCRDict crstrm
  in getRefs isRootRef $ crdict

isRootRef (PdfName "/Root", ObjRef x) = True
isRootRef (_,_) = False

getRefs :: ((Obj,Obj) -> Bool) -> Dict -> Maybe Int
getRefs pred dict = case find pred dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing

-- find Info

findTrailer bs = do
  case parseTrailer bs of
    Just d -> d
    Nothing -> []

infoRef bs = case parseTrailer bs of
  Just dict -> getRefs isInfoRef dict
  Nothing -> error "No ref for info"

isInfoRef (PdfName "/Info", ObjRef x) = True
isInfoRef (_,_) = False


-- expand PDF 1.5 Object Stream 

expandObjStm :: [PDFObj] -> [PDFObj]
expandObjStm os = concat $ map objStm os

objStm :: PDFObj -> [PDFObj]
objStm (n, obj) = case findDictOfType "/ObjStm" obj of
  Nothing -> [(n,obj)]
  Just _  -> getPdfObjStm n $ BSL.toStrict $ rawStream obj
  
refOffset :: Parser ([(Int, Int)], String)
refOffset = spaces *> ((,) 
                       <$> many1 ((\r o -> (read r :: Int, read o :: Int))
                                  <$> (many1 digit <* spaces) 
                                  <*> (many1 digit <* spaces))
                       <*> many1 anyChar)

getPdfObjStm n s = 
  let (location, objstr) = case parseOnly refOffset s of
        Right val -> val
        Left err  -> error $ "Failed to parse Object Stream: "
  in map (\(r,o) -> (r, parseDict $ BS.pack $ drop o objstr)) location
    where parseDict s' = case parseOnly pdfdictionary s' of
            Right obj -> [obj]
            Left  _   -> case parseOnly pdfarray s' of
              Right obj -> [obj]
              Left _ -> case parseOnly pdfletters s' of
                Right obj -> [obj]
                Left err -> error $ (show err) ++ ":\n   Failed to parse obj around; \n"
                              ++ (show $ BS.take 100 s')
