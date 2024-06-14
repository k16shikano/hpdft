{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.DocumentStructure
Description : Function to walk around Document Structure of a PDF file
Copyright   : (c) Keiichiro Shikano, 2020
License     : MIT
Maintainer  : k16.shikano@gmail.com
-}

module PDF.DocumentStructure
       ( expandObjStm
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

import Data.Char (chr, isDigit)
import Data.List (find, elem)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Numeric (readDec)

import Data.Attoparsec.ByteString.Char8 hiding (take, isDigit)
import Data.Attoparsec.ByteString.Char8 as P (take, takeWhile)
import Data.Attoparsec.Combinator
import Control.Applicative
import Codec.Compression.Zlib (decompress)

import Debug.Trace

import PDF.Definition
import PDF.Object
import PDF.ContentStream (parseStream, parseColorSpace)
import PDF.Cmap (parseCMap)
import qualified PDF.OpenType as OpenType
import qualified PDF.CFF as CFF
import qualified PDF.Type1 as Type1

spaces = skipSpace
oneOf = satisfy . inClass
noneOf = satisfy . notInClass

-- find objects

findObjs :: BS.ByteString -> [PDFBS]
findObjs contents = case parseOnly (many1 pdfObj) contents of
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
  Just (PdfStream strm) -> rawStream' (fromMaybe [] $ findDict objs) strm
  Nothing               -> BSL.pack $ show objs
  where
    isStream (PdfStream s) = True
    isStream _             = False

    rawStream' :: Dict -> BSL.ByteString -> BSL.ByteString
    rawStream' d s = streamFilter d s

    streamFilter d = case find withFilter d of
      Just (PdfName "/Filter", PdfName "/FlateDecode")
        -> decompress
      Just (PdfName "/Filter", PdfName f)
        -> error $ "Unknown Stream Compression: " ++ f -- need fix
      Just _ -> error $ "No Stream Compression Filter."
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


-- find root ref from Trailer Dictionary

readDec' bs = case readDec $ BS.unpack bs of
  [(n,_)] -> n
  _ -> error $ show bs

findTrailer :: BS.ByteString -> Maybe Dict
findTrailer bs = case BS.breakEnd (== '\n') bs of
  (source, eofLine)
    | "%%EOF" `BS.isPrefixOf` eofLine
      -> let dictXref = findTrailerDictXREF $ BS.drop (getXrefOffset source) bs
         in Just $ fst (trace (show dictXref) dictXref)
    | source == "" -> Nothing
    | otherwise -> findTrailer (BS.init bs)
  where 
    getXrefOffset bs = case BS.breakEnd (== '\n') (BS.init bs) of
      (_, nstr) -> readDec' nstr

findTrailerDictXREF :: BS.ByteString -> (Dict, [XREF])
findTrailerDictXREF xrefTrailer = case BS.breakSubstring "trailer" xrefTrailer of
  (xref, trailer) -> case parseOnly (pdfdictionary <* spaces) (BS.drop 7 trailer) of
    Left  err  -> error $ show (BS.take 100 trailer)
    Right (PdfDict dict) -> (dict, parseXref xref)

parseXref :: BS.ByteString -> [XREF]
parseXref xref = case parseOnly xrefParser xref of
  Left  err  -> error $ show (BS.take 100 xref)
  Right xs -> concatMap concatSubsections xs
  
  where 
    xrefParser = do
      string "xref" >> spaces
      es <- many1 subsections
      return $ es
    subsections = do
      begin <- P.takeWhile isDigit <* spaces
      num <- P.takeWhile isDigit <* spaces
      es <- many1 entries 
      return $ (readDec' begin, readDec' num, es)
    entries = do
      offset <- P.take 10 <* spaces
      gennum <- P.take 5 <* spaces
      status <- P.take 1 <* string "\r\n"
      return $ (readDec' offset, forn status)
    forn "f" = False
    forn "n" = True
    forn _ = error "xref is neither f nor n"

    concatSubsections (objn, 1, e:[]) = [(objn, fst e, snd e)]
    concatSubsections (objn, 1, e:_)  = error $ show e
    concatSubsections (objn, i, e:es) = (objn, fst e, snd e):(concatSubsections (objn+1, i-1, es))
    concatSubsections (_, _, e)  = error $ show e

rootRef :: BS.ByteString -> Maybe Int
rootRef bs = case findTrailer bs of
  Just dict -> findRefs isRootRef dict
  Nothing   -> rootRefFromCRStream bs

rootRefFromCRStream :: BS.ByteString -> Maybe Int
rootRefFromCRStream bs =
  let offset = (read . BS.unpack . head . drop 1 . reverse . BS.lines $ bs) :: Int
      crstrm = snd . head . findObjs $ BS.drop offset bs
      crdict = fst $ findTrailerDictXREF crstrm
  in findRefs isRootRef $ crdict

isRootRef (PdfName "/Root", ObjRef x) = True
isRootRef (_,_) = False

findRefs :: ((Obj,Obj) -> Bool) -> Dict -> Maybe Int
findRefs pred dict = case find pred dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing


-- find Info

infoRef bs = case findTrailer bs of
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
    Just (PdfDict d') -> d'
    Just (ObjRef x) -> case findDictByRef x objs of
                         Just d' -> d'
                         otherwise -> error "cannot find /Font dictionary"
    otherwise -> trace (show d) $ []
  Nothing -> []

findResourcesDict :: Dict -> [PDFObj] -> Maybe Dict
findResourcesDict dict objs = case find resources dict of
  Just (_, ObjRef x)  -> findDictByRef x objs
  Just (_, PdfDict d) -> Just d
  otherwise -> error (show dict)
  where
    resources (PdfName "/Resources", _) = True
    resources _                         = False


encoding :: Int -> [PDFObj] -> Encoding
encoding x objs = case subtype of
  Just (PdfName "/Type0") -> case encoding of
    Just (PdfName "/Identity-H") -> head $ cidSysInfo descendantFonts
    -- TODO" when /Encoding is stream of CMap
    Just (PdfName s) -> error $ "Unknown Encoding " ++ (show s) ++ " for a Type0 font. Check " ++ show x
    _ -> error $ "Something wrong with a Type0 font. Check " ++ (show x)
  Just (PdfName "/Type1") -> case encoding of
    Just (ObjRef r) -> case findObjFromDictWithRef r "/Differences" objs of
                     Just (PdfArray arr) -> charDiff arr
                     _ -> error "No /Differences"
    Just (PdfDict d) -> case findObjFromDict d "/Differences" of
                     Just (PdfArray arr) -> charDiff arr
                     _ -> error "No /Differences"
    Just (PdfName "/MacRomanEncoding") -> NullMap
    Just (PdfName "/MacExpertEncoding") -> NullMap
    Just (PdfName "/WinAnsiEncoding") -> NullMap
    -- TODO: FontFile (Type 1), FontFile2 (TrueType), FontFile3 (Other than Type1C)
    _ -> case findObjFromDict (fontDescriptor' x) "/FontFile3" of
           Just (ObjRef fontfile) ->
             CFF.encoding $ BSL.toStrict $ rawStreamByRef objs fontfile
           _ -> case findObjFromDict (fontDescriptor' x) "/FontFile" of
             Just (ObjRef fontfile) ->
               Type1.encoding $ BSL.toStrict $ rawStreamByRef objs fontfile
             _ -> NullMap
  -- TODO
  Just (PdfName "/Type2") -> NullMap
  Just (PdfName "/Type3") -> NullMap
  _ -> NullMap

  where
    subtype = get "/Subtype"
    encoding = get "/Encoding"
    toUnicode = get "/ToUnicode" 

    get s = findObjFromDictWithRef x s objs

    -- Should be an array (or ref to an array) containing refs
    descendantFonts :: [Obj]
    descendantFonts = case findObjFromDictWithRef x "/DescendantFonts" objs of
      Just (PdfArray dfrs) -> dfrs
      Just (ObjRef r) -> case findObjsByRef r objs of
        Just [(PdfArray dfrs)] -> dfrs
        _ -> error $ "Can not find /DescendantFonts entries in " ++ show r
      _ -> error $ "Can not find /DescendantFonts itself in " ++ show x 

    cidSysInfo :: [Obj] -> [Encoding]
    cidSysInfo [] = []
    cidSysInfo ((ObjRef r):rs) = (cidSysInfo' r):(cidSysInfo rs)
    cidSysInfo' dfr = case findObjFromDictWithRef dfr "/CIDSystemInfo" objs of
      Just (PdfDict dict) -> getCIDSystemInfo dict
      Just (ObjRef r) -> case findDictByRef r objs of
                           Just dict -> getCIDSystemInfo dict
                           _ -> error $ "Can not find /CIDSystemInfo entries in" ++ show r
      _ -> error $ "Can not find /CidSystemInfo itself " ++ show dfr

    fontDescriptor :: [Obj] -> [Dict]
    fontDescriptor [] = []
    fontDescriptor ((ObjRef r):rs) = (fontDescriptor' r):(fontDescriptor rs)
    fontDescriptor' :: Int -> Dict
    fontDescriptor' fdr = case findObjFromDictWithRef fdr "/FontDescriptor" objs of
      Just (ObjRef r) -> case findDictByRef r objs of
                           Just dict -> dict
                           _ -> error $ "No /FontDescriptor entries in " ++ show r
      _ -> error $ "Can not find /FontDescriptor itself in " ++ show fdr

    getCIDSystemInfo d =
      let registry = case findObjFromDict d "/Registry" of
                       Just (PdfText r) -> r
                       otherwise -> error "Can not find /Registry"
          ordering = case findObjFromDict d "/Ordering" of
                       Just (PdfText o) -> o
                       othserwise -> error "Can not find /Ordering"
          supplement = case findObjFromDict d "/Supplement" of
                         Just (PdfNumber s) -> s
                         otherwise -> error "Can not find /Supprement"
          cmap = registry ++ "-" ++ ordering -- ex. "Adobe-Japan1"
      in if cmap == "Adobe-Japan1"
         then CIDmap cmap
         else WithCharSet ""


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

