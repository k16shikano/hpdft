{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.DocumentStructure
Description : Function to walk around Document Structure of a PDF file
Copyright   : (c) Keiichiro Shikano, 2020
License     : MIT
Maintainer  : k16.shikano@gmail.com
-}

module PDF.DocumentStructure
       ( parseTrailer
       , expandObjStm
       , rootRef
       , contentsStream
       , rawStreamByRef
       , findKids
       , findPages
       , findDict
       , findDictByRef
       , findDictOfType
       , findObjFromDict
       , findObjFromDictWithRef
       , findObjsByRef
       , findObjs
       , findTrailer
       , rawStream
       ) where

import Data.Char (chr)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.Text as T

import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Attoparsec.Combinator
import Control.Applicative
import Codec.Compression.Zlib (decompress)

import Debug.Trace

import PDF.Definition
import PDF.Object
import PDF.ContentStream (parseStream, parseColorSpace)
import PDF.Cmap (parseCMap)
import qualified PDF.OpenType as OpenType

spaces = skipSpace
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

-- find objects

findObjs :: BS.ByteString -> [PDFBS]
findObjs contents = case parseOnly (many1 pdfObj) contents of
  Left  err -> []
  Right rlt -> rlt

findXref :: BS.ByteString -> String
findXref contents = case parseOnly (xref) contents of
  Left  err -> []
  Right rlt -> rlt

findObjsByRef :: Int -> [PDFObj] -> Maybe [Obj]
findObjsByRef x pdfobjs = case find (isRefObj (Just x)) pdfobjs of
  Just (_,objs) -> Just objs
  Nothing -> Nothing
  where
    isRefObj (Just x) (y, objs) = if x==y then True else False
    isRefObj _ _ = False

findObjFromDictWithRef :: Int -> String -> [PDFObj] -> Maybe Obj
findObjFromDictWithRef ref name objs = case findDictByRef ref objs of 
  Just d -> findObjFromDict d name
  Nothing -> Nothing
  
findObjFromDict :: Dict -> String -> Maybe Obj
findObjFromDict d name = case find isName d of
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

findPages :: Dict -> Maybe Int
findPages dict = case find isPagesRef dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing
  where
    isPagesRef (PdfName "/Pages", ObjRef x) = True
    isPagesRef (_,_)                        = False
    
findKids :: Dict -> Maybe [Int]
findKids dict = case find isKidsRefs dict of
  Just (_, PdfArray arr) -> Just (parseRefsArray arr)
  Nothing                -> Nothing
  where 
    isKidsRefs (PdfName "/Kids", PdfArray x) = True
    isKidsRefs (_,_)                         = False

contentsStream :: Dict -> PSR -> [PDFObj] -> PDFStream
contentsStream dict st objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> getContentArray arr
  Just (PdfName "/Contents", ObjRef r) ->
    case findObjsByRef r objs of
      Just [PdfArray arr] -> getContentArray arr
      Just _ -> getContent r
      Nothing -> error "No content to be shown"
  Nothing -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _ = False

    getContentArray arr = parseContentStream dict st objs $
                          BSL.concat $ map (rawStreamByRef objs) (parseRefsArray arr)
    getContent r = parseContentStream dict st objs $ rawStreamByRef objs r

parseContentStream :: Dict -> PSR -> [PDFObj] -> BSL.ByteString -> PDFStream
parseContentStream dict st objs s = 
  parseStream (st {fontmaps=fontdict, cmaps=cmap}) s
  where fontdict = findFontEncoding dict objs
        cmap = findCMap dict objs

rawStreamByRef :: [PDFObj] -> Int -> BSL.ByteString
rawStreamByRef pdfobjs x = case findObjsByRef x pdfobjs of
  Just objs -> rawStream objs
  Nothing  -> error "No object with stream to be shown"

rawStream :: [Obj] -> BSL.ByteString
rawStream objs = case find isStream objs of
  Just (PdfStream strm) -> streamFilter strm
  Nothing               -> BSL.pack $ show objs
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

contentsColorSpace :: Dict -> PSR -> [PDFObj] -> [T.Text]
contentsColorSpace dict st objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> concat $ map (parseColorSpace (st {xcolorspaces=xobjcs}) . rawStreamByRef objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> parseColorSpace (st {xcolorspaces=xobjcs}) $ rawStreamByRef objs x
  Nothing                                  -> error "No content to be shown"
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False
    xobjcs = findXObjectColorSpace dict objs


-- find XObject

findXObjectColorSpace d os = xobjColorSpaceMap (findXObject d os) os

xobjColorSpaceMap dict objs = map pairwise dict
  where
    pairwise (PdfName n, ObjRef r) = xobjColorSpace r objs
    pairwise x = ""

findXObject dict objs = case findResourcesDict dict objs of
  Just d -> case findObjFromDict d "/XObject" of
    Just (PdfDict d) -> d
    otherwise -> []
  Nothing -> []

xobjColorSpace :: Int -> [PDFObj] -> String
xobjColorSpace x objs = case findObjFromDictWithRef x "/ColorSpace" objs of
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
  Just dict -> findRefs isRootRef dict
  Nothing   -> rootRefFromCRStream bs

rootRefFromCRStream :: BS.ByteString -> Maybe Int
rootRefFromCRStream bs =
  let offset = (read . BS.unpack . head . drop 1 . reverse . BS.lines $ (trace (show bs) bs)) :: Int
      crstrm = snd . head . findObjs $ BS.drop offset bs
      crdict = parseCRDict crstrm
  in findRefs isRootRef $ crdict

isRootRef (PdfName "/Root", ObjRef x) = True
isRootRef (_,_) = False

findRefs :: ((Obj,Obj) -> Bool) -> Dict -> Maybe Int
findRefs pred dict = case find pred dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing


-- find Info

findTrailer bs = do
  case parseTrailer bs of
    Just d -> d
    Nothing -> []

infoRef bs = case parseTrailer bs of
  Just dict -> findRefs isInfoRef dict
  Nothing -> error "No ref for info"

isInfoRef (PdfName "/Info", ObjRef x) = True
isInfoRef (_,_) = False


-- expand PDF 1.5 Object Stream 

expandObjStm :: [PDFObj] -> [PDFObj]
expandObjStm os = concat $ map objStm os

objStm :: PDFObj -> [PDFObj]
objStm (n, obj) = case findDictOfType "/ObjStm" obj of
  Nothing -> [(n,obj)]
  Just _  -> pdfObjStm n $ BSL.toStrict $ rawStream obj
  
refOffset :: Parser ([(Int, Int)], String)
refOffset = spaces *> ((,) 
                       <$> many1 ((\r o -> (read r :: Int, read o :: Int))
                                  <$> (many1 digit <* spaces) 
                                  <*> (many1 digit <* spaces))
                       <*> many1 anyChar)

pdfObjStm n s = 
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


-- make fontmap from page's /Resources (see 3.7.2 of PDF Ref.)

findFontEncoding d os = findEncoding (fontObjs d os) os

findEncoding :: Dict -> [PDFObj] -> [(String, Encoding)]
findEncoding dict objs = map pairwise dict
  where 
    pairwise (PdfName n, ObjRef r) = (n, encoding r objs)
    pairwise x = ("", NullMap)

fontObjs :: Dict -> [PDFObj] -> Dict
fontObjs dict objs = case findResourcesDict dict objs of
  Just d -> case findObjFromDict d "/Font" of
    Just (PdfDict d) -> d
    otherwise -> []
  Nothing -> []

findResourcesDict :: Dict -> [PDFObj] -> Maybe Dict
findResourcesDict dict objs = case find resources dict of
  Just (_, ObjRef x)  -> findDictByRef x objs
  Just (_, PdfDict d) -> Just d
  otherwise -> error (show dict)
  where
    resources (PdfName "/Resources", _) = True
    resources _                         = False

-- Needs rewrite!
encoding :: Int -> [PDFObj] -> Encoding
encoding x objs = case findObjFromDictWithRef x "/Encoding" objs of
  Just (ObjRef ref) -> case findObjFromDictWithRef ref "/Differences" objs of
    Just (PdfArray arr) -> charDiff arr
    otherwise -> trace "no /differences" NullMap
  Just (PdfName "/StandardEncoding") -> NullMap
  Just (PdfName "/MacRomanEncoding") -> NullMap
  Just (PdfName "/MacExpertEncoding") -> NullMap
  Just (PdfName "/WinAnsiEncoding") -> NullMap

  otherwise -> case findObjFromDictWithRef x "/ToUnicode" objs of
    Just (ObjRef ref) -> case findObjFromDictWithRef ref "/CharSet" objs of
      Just (PdfText str) -> WithCharSet str
      otherwise -> WithCharSet ""
    otherwise -> case findObjFromDictWithRef x "/DescendantFonts" objs of -- needs CID to Unicode map
      Just (ObjRef ref) -> case findObjsByRef ref objs of
        Just [(PdfArray ((ObjRef subref):_))] -> case findObjFromDictWithRef subref "/CIDSystemInfo" objs of
          Just (ObjRef inforef) -> case findObjFromDictWithRef inforef "/Registry" objs of
            Just (PdfText "Adobe") -> case findObjFromDictWithRef inforef "/Ordering" objs of
              Just (PdfText "Japan1") -> case findObjFromDictWithRef inforef "/Supplement" objs of
                Just (PdfNumber _) -> CIDmap "Adobe-Japan1"
                _ -> trace (show inforef) defaultCIDMap
              _ -> trace (show inforef) defaultCIDMap
            _ -> trace (show inforef) defaultCIDMap
          _ -> trace (show subref ++ " no /cidsysteminfoy. using default...") defaultCIDMap
        _ -> trace (show ref ++ " no array in /descendantfonts. using default...") defaultCIDMap
      _ -> case findObjFromDictWithRef x "/FontDescriptor" objs of
        Just (ObjRef ref) -> case findObjFromDictWithRef ref "/FontFile3" objs of
          Just (ObjRef fontfile) -> NullMap
          otherwise -> trace (show x ++ " no /descendantfonts. using default...") defaultCIDMap
        _ -> NullMap
  where
    defaultCIDMap = NullMap -- CIDmap "Adobe-Japan1"


charDiff :: [Obj] -> Encoding
charDiff objs = Encoding $ charmap objs 0
  where charmap (PdfNumber x : PdfName n : xs) i = 
          if i < truncate x then 
            (chr $ truncate x, n) : (charmap xs $ incr x)
          else 
            (chr $ i, n) : (charmap xs $ i+1)
        charmap (PdfName n : xs) i = (chr i, n) : (charmap xs $ i+1)
        charmap [] i               = []
        incr x = (truncate x) + 1


findCMap :: Dict -> [PDFObj] -> [(String, CMap)]
findCMap d objs = map pairwise (fontObjs d objs)
  where
    pairwise (PdfName n, ObjRef r) = (n, toUnicode r objs)
    pairwise x = ("", [])

toUnicode :: Int -> [PDFObj] -> CMap
toUnicode x objs =
  case findObjFromDictWithRef x "/ToUnicode" objs of
    Just (ObjRef ref) ->
      parseCMap $ rawStreamByRef objs ref
    otherwise -> noToUnicode x objs

noToUnicode x objs = 
  case findObjFromDictWithRef x "/DescendantFonts" objs of
    Just (ObjRef ref) ->
      case findObjsByRef ref objs of
        Just [(PdfArray ((ObjRef subref):_))] ->
          case findObjFromDictWithRef subref "/FontDescriptor" objs of
            Just (ObjRef desc) ->
              case findObjFromDictWithRef desc "/FontFile2" objs of
                Just (ObjRef fontfile) ->
                  OpenType.cmap $ BSL.toStrict $ rawStreamByRef objs fontfile
                otherwise -> []
            otherwise -> []
        otherwise -> []
    otherwise -> []

