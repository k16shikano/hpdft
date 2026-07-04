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
       , findObjs'
       , findTrailer
       , indexPDFObjs
       , rawStream
       ) where

import Data.Char (chr, isDigit, ord)
import Data.List (find, elem, foldl')
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Numeric (readDec)

import Data.Attoparsec.ByteString.Char8 hiding (take, isDigit)
import Data.Attoparsec.ByteString.Char8 as P (take, takeWhile, takeWhile1)
import Data.Attoparsec.Combinator
import Control.Applicative
import Control.Monad (replicateM)
import Codec.Compression.Zlib (decompress)
import qualified Data.Map as Map
import Data.Map (Map)

import Debug.Trace

import PDF.Definition
import PDF.Object
import PDF.Encrypt (Security, decryptStream)
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

findObjs' :: BS.ByteString -> [PDFBS]
findObjs' contents = case findTrailer' contents of
  Just (_, xref) -> Map.toAscList $ Map.map (extractObjBody contents) xref
  Nothing -> findObjs contents

extractObjBody :: BS.ByteString -> Int -> BS.ByteString
extractObjBody contents offset =
  case BS.breakSubstring "endobj" (BS.drop offset contents) of
    (before, _) ->
      let (_, body) = BS.breakSubstring " obj" before
      in BS.dropWhile isPdfSpace body

isPdfSpace :: Char -> Bool
isPdfSpace w = w `elem` ['\0', '\t', '\n', '\f', '\r', ' ']

findObjsByRef :: Int -> PDFObjIndex -> Maybe [Obj]
findObjsByRef = Map.lookup

indexPDFObjs :: [PDFObj] -> PDFObjIndex
indexPDFObjs = Map.fromList

findObjFromDictWithRef :: Int -> String -> PDFObjIndex -> Maybe Obj
findObjFromDictWithRef ref name objs = case findDictByRef ref objs of 
  Just d -> findObjFromDict d name
  Nothing -> Nothing
  
findObjFromDict :: Dict -> String -> Maybe Obj
findObjFromDict d name = case find isName d of
  Just (_, o) -> Just o
  otherwise -> Nothing
  where isName (PdfName n, _) = if name == n then True else False
        isName _              = False

findDictByRef :: Int -> PDFObjIndex -> Maybe Dict
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

contentsStream :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> PDFStream
contentsStream dict st sec objs = case find contents dict of
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

    getContentArray arr = parseContentStream dict st sec objs $
                          BSL.concat $ map (rawStreamByRef sec objs) (parseRefsArray arr)
    getContent r = parseContentStream dict st sec objs $ rawStreamByRef sec objs r

parseContentStream :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> BSL.ByteString -> PDFStream
parseContentStream dict st sec objs s = 
  parseStream (st {fontmaps=fontdict, cmaps=cmap}) s
  where fontdict = findFontEncoding dict sec objs
        cmap = findCMap dict sec objs

rawStreamByRef :: Maybe Security -> PDFObjIndex -> Int -> BSL.ByteString
rawStreamByRef sec pdfobjs x = case findObjsByRef x pdfobjs of
  Just objs -> rawStream sec x objs
  Nothing  -> error "No object with stream to be shown"

rawStream :: Maybe Security -> Int -> [Obj] -> BSL.ByteString
rawStream sec objNum objs = case find isStream objs of
  Just (PdfStream strm) -> rawStream' sec objNum (fromMaybe [] $ findDict objs) strm
  Nothing               -> BSL.pack $ show objs
  where
    isStream (PdfStream s) = True
    isStream _             = False

    rawStream' :: Maybe Security -> Int -> Dict -> BSL.ByteString -> BSL.ByteString
    rawStream' sec' objNum' d s =
      BSL.fromStrict $ decodeStreamBytes d $ BSL.fromStrict $
        decryptStream sec' objNum' 0 $ BSL.toStrict s

decodeStreamBytes :: Dict -> BSL.ByteString -> BS.ByteString
decodeStreamBytes d s =
  applyPredictor d $ BSL.toStrict $ case find isFilter d of
    Just (PdfName "/Filter", PdfName "/FlateDecode") -> decompress s
    Nothing -> s
    Just (PdfName "/Filter", PdfName f) ->
      error $ "Unknown Stream Compression: " ++ f
    Just _ -> error "No Stream Compression Filter."
  where
    isFilter (PdfName "/Filter", _) = True
    isFilter _                      = False

applyPredictor :: Dict -> BS.ByteString -> BS.ByteString
applyPredictor d bs = case findObjFromDict d "/DecodeParms" of
  Just (PdfDict parms) ->
    case findObjFromDict parms "/Predictor" of
      Just (PdfNumber p) | truncate p >= 12 ->
        case findObjFromDict parms "/Columns" of
          Just (PdfNumber c) -> decodePNGPredictors bs (truncate c)
          _ -> bs
      _ -> bs
  _ -> bs

decodePNGPredictors :: BS.ByteString -> Int -> BS.ByteString
decodePNGPredictors bs columns = go BS.empty bs
  where
    go _ rest | BS.null rest = BS.empty
    go prev rest =
      let filt = BS.head rest
          enc  = BS.take columns (BS.drop 1 rest)
          rest' = BS.drop (1 + columns) rest
          prevRow = if BS.null prev then BS.replicate columns (chr 0) else prev
          row = pngFilter filt enc prevRow
      in BS.append row (go row rest')

pngFilter :: Char -> BS.ByteString -> BS.ByteString -> BS.ByteString
pngFilter filt row prev
  | ord filt == 0 = row
  | ord filt == 1 = pngSub row
  | ord filt == 2 = pngUp row prev
  | otherwise = error $ "unsupported PNG predictor " ++ show (ord filt)

pngUp :: BS.ByteString -> BS.ByteString -> BS.ByteString
pngUp row prev = BS.pack $ zipWith addByte (BS.unpack row) (BS.unpack prev)
  where addByte a b = chr ((ord a + ord b) `mod` 256)

pngSub :: BS.ByteString -> BS.ByteString
pngSub row =
  BS.pack $ snd $ foldl' (\(prev, out) x ->
    let n = (ord x + prev) `mod` 256 in (n, out ++ [chr n])) (0, []) (BS.unpack row)

contentsColorSpace :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> [T.Text]
contentsColorSpace dict st sec objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> concat $ map (parseColorSpace (st {xcolorspaces=xobjcs}) . rawStreamByRef sec objs) (parseRefsArray arr)
  Just (PdfName "/Contents", ObjRef x)     -> parseColorSpace (st {xcolorspaces=xobjcs}) $ rawStreamByRef sec objs x
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

xobjColorSpace :: Int -> PDFObjIndex -> String
xobjColorSpace x objs = case findObjFromDictWithRef x "/ColorSpace" objs of
  Just (PdfName cs) -> cs
  otherwise -> ""


-- find root ref from Trailer Dictionary

readDec' bs = case readDec $ BS.unpack bs of
  [(n,_)] -> n
  _ -> error $ show bs

isPdfEofLine :: BS.ByteString -> Bool
isPdfEofLine line =
  case BS.dropWhile isSpaceChar line of
    rest | "%%EOF" `BS.isPrefixOf` rest ->
      BS.all isSpaceChar (BS.drop 5 rest)
    _ -> False

getStartxrefOffset :: BS.ByteString -> Int
getStartxrefOffset source =
  let trimmed = BS.dropWhileEnd isSpaceChar source
      numLine = case BS.breakEnd (== '\n') trimmed of
        (_, n) | not (BS.null n) -> BS.dropWhile isSpaceChar n
        _ -> trimmed
  in readDec' $ BS.takeWhile (\c -> c >= '0' && c <= '9') numLine

isSpaceChar :: Char -> Bool
isSpaceChar c = c `elem` (" \t\r\n" :: String)

mergeXRefStm :: BS.ByteString -> Dict -> XREF -> XREF
mergeXRefStm all dict xref = case findObjFromDict dict "/XRefStm" of
  Just (PdfNumber n) ->
    let stm = snd $ findTrailerDictXREFStream $ BS.drop (truncate n) all
    in Map.union xref stm
  _ -> xref

findTrailer :: BS.ByteString -> Maybe Dict
findTrailer bs = case BS.breakEnd (== '\n') bs of
  (source, eofLine)
    | isPdfEofLine eofLine
      -> Just $ fst $ findTrailerDictXREF $ BS.drop (getStartxrefOffset source) bs
    | source == "" -> Nothing
    | otherwise -> findTrailer (BS.init bs)

findTrailer' :: BS.ByteString -> Maybe (Dict, XREF)
findTrailer' bs = case BS.breakEnd (== '\n') bs of
  (source, eofLine)
    | isPdfEofLine eofLine
      -> case findTrailerDictXREF $ BS.drop (getStartxrefOffset source) bs of
           (dict, xref) ->
             let xref' = mergeXRefStm bs dict xref
             in case find isPrev dict of
               Just (PdfName "/Prev", PdfNumber x) -> xrefs (truncate x) bs (dict, xref')
               Nothing -> Just (dict, xref')
    | source == "" -> Nothing
    | otherwise -> findTrailer' (BS.init bs)
  where
    isPrev (PdfName "/Prev", _) = True
    isPrev (_,_) = False

    xrefs n all (dict, sofar) = case findTrailerDictXREF $ BS.drop n all of
      (dict', xref) ->
        let xref' = mergeXRefStm all dict' xref
        in case find isPrev dict' of
          Just (PdfName "/Prev", PdfNumber x) -> xrefs (truncate x) all (dict, Map.union xref' sofar)
          Nothing -> Just (dict, Map.union xref' sofar)

findTrailerDictXREF :: BS.ByteString -> (Dict, XREF)
findTrailerDictXREF xrefTrailer =
  let trimmed = BS.dropWhile isPdfSpace xrefTrailer
  in case BS.take 4 trimmed of
       "xref" -> findTrailerDictXREFTable xrefTrailer
       _      -> findTrailerDictXREFStream xrefTrailer

findTrailerDictXREFTable :: BS.ByteString -> (Dict, XREF)
findTrailerDictXREFTable xrefTrailer = case BS.breakSubstring "trailer" xrefTrailer of
  (xref, trailer) -> case parseOnly (pdfdictionary <* spaces) (BS.drop 7 trailer) of
    Left  err  -> error $ show (BS.take 100 trailer)
    Right (PdfDict dict) -> (dict, parseXref xref)

findTrailerDictXREFStream :: BS.ByteString -> (Dict, XREF)
findTrailerDictXREFStream blob = case parseOnly xrefStreamObject blob of
  Left err  -> error $ "xref stream: " ++ show err ++ ": " ++ show (BS.take 80 blob)
  Right (dict, s) -> (dict, xrefStreamToMap dict s)

parseXrefBlob :: BS.ByteString -> XREF
parseXrefBlob bs =
  let trimmed = BS.dropWhile isPdfSpace bs
  in case BS.take 4 trimmed of
       "xref" -> parseXref bs
       _      -> parseXrefStream bs

parseXrefStream :: BS.ByteString -> XREF
parseXrefStream bs = case parseOnly xrefStreamObject bs of
  Left err  -> error $ "xref stream: " ++ show err ++ ": " ++ show (BS.take 80 bs)
  Right (dict, s) -> xrefStreamToMap dict s

xrefStreamObject :: Parser (Dict, BSL.ByteString)
xrefStreamObject = do
  spaces
  _ <- many1 digit
  spaces
  _ <- many1 digit
  string " obj"
  spaces
  dictObj <- pdfdictionary
  d <- case dictObj of
    PdfDict d' -> return d'
    _          -> fail "expected dictionary in xref stream object"
  spaces
  s <- readStreamByLength d
  spaces
  optional (string "endobj")
  return (d, s)

readStreamByLength :: Dict -> Parser BSL.ByteString
readStreamByLength dict = do
  string "stream"
  _ <- string "\r\n" <|> string "\n" <|> string "\r"
  len <- case findObjFromDict dict "/Length" of
    Just (PdfNumber n) -> return (truncate n)
    _ -> fail "stream without /Length"
  bs <- P.take len
  optional (try $ string "\r\n" <|> string "\n" <|> string "\r")
  return $ BSL.fromStrict bs

xrefStreamToMap :: Dict -> BSL.ByteString -> XREF
xrefStreamToMap dict s =
  let ws = wFields dict
      sections = indexSections dict
      raw = decodeStreamBytes dict s
      entries = xrefStreamEntries ws sections raw
  in Map.fromList $ [(n, off) | (n, typ, off) <- entries, typ == 1]

wFields :: Dict -> (Int, Int, Int)
wFields d = case findObjFromDict d "/W" of
  Just (PdfArray [PdfNumber a, PdfNumber b, PdfNumber c]) ->
    (truncate a, truncate b, truncate c)
  _ -> error "xref stream missing /W"

indexSections :: Dict -> [(Int, Int)]
indexSections d = case findObjFromDict d "/Index" of
  Just (PdfArray arr) -> indexPairs arr
  Nothing -> case findObjFromDict d "/Size" of
    Just (PdfNumber s) -> [(0, truncate s)]
    _ -> error "xref stream missing /Size"
  where
    indexPairs (PdfNumber a : PdfNumber b : xs) =
      (truncate a, truncate b) : indexPairs xs
    indexPairs [] = []
    indexPairs _  = error "malformed /Index in xref stream"

xrefStreamEntries :: (Int, Int, Int) -> [(Int, Int)] -> BS.ByteString -> [(Int, Int, Int)]
xrefStreamEntries widths sections raw =
  fst $ foldl' (parseSection widths) ([], raw) sections
  where
    parseSection (w0, w1, w2) (acc, bs) (start, count) =
      let (ents, rest) = parseN (w0, w1, w2) start count bs
      in (acc ++ ents, rest)
    parseN (w0, w1, w2) start count bs =
      foldl' (\(es, b) objNum ->
                let (typ, b1) = readField w0 b
                    (f2, b2)  = readField w1 b1
                    (_, b3)   = readField w2 b2
                in ((objNum, typ, f2) : es, b3))
             ([], bs) [start .. start + count - 1]

readField :: Int -> BS.ByteString -> (Int, BS.ByteString)
readField 0 bs = (0, bs)
readField w bs = (bytesToInt (BS.take w bs), BS.drop w bs)

bytesToInt :: BS.ByteString -> Int
bytesToInt = BS.foldl' (\acc w -> acc * 256 + ord w) 0

parseXref :: BS.ByteString -> XREF
parseXref xref = case parseOnly xrefParser xref of
  Left  err  -> error $ show (BS.take 100 xref)
  Right xs -> Map.fromList $ map dropFN $ filter (\(_,_,inUse) -> inUse) $ concatMap concatSubsections xs
  
  where 
    xrefParser = do
      string "xref" >> spaces
      es <- (:) <$> subsections <*> many (try subsections)
      return es
    subsections = do
      begin <- takeWhile1 isDigit <* spaces
      num <- takeWhile1 isDigit <* spaces
      let count = readDec' num
      es <- replicateM count entries
      return $ (readDec' begin, count, es)
    entries = do
      offset <- P.take 10 <* spaces
      gennum <- P.take 5 <* spaces
      status <- P.take 1 <* spaces
      _ <- optional (string "\r\n" <|> string "\n" <|> string "\r")
      return $ (readDec' offset, forn status)
    forn "f" = False
    forn "n" = True
    forn _ = error "xref is neither f nor n"

    concatSubsections (objn, 0, []) = []
    concatSubsections (objn, 1, e:[]) = [(objn, fst e, snd e)]
    concatSubsections (objn, 1, e:_)  = error $ show e
    concatSubsections (objn, i, e:es) = (objn, fst e, snd e):(concatSubsections (objn+1, i-1, es))
    concatSubsections (_, _, e)  = error $ show e
    
    dropFN (n, offset, fn) = (n, offset)

rootRef :: BS.ByteString -> Maybe Int
rootRef bs = case findTrailer bs of
  Just dict -> findRefs isRootRef dict
  Nothing   -> rootRefFromCRStream bs

rootRefFromCRStream :: BS.ByteString -> Maybe Int
rootRefFromCRStream bs =
  let offset = (read . BS.unpack . head . drop 1 . reverse . BS.lines $ bs) :: Int
      crstrm = snd . head . findObjs' $ BS.drop offset bs
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

expandObjStm :: Maybe Security -> [PDFObj] -> [PDFObj]
expandObjStm sec os = concat $ map (objStm sec) os

objStm :: Maybe Security -> PDFObj -> [PDFObj]
objStm sec (n, obj) = case findDictOfType "/ObjStm" obj of
  Nothing -> [(n,obj)]
  Just _  -> pdfObjStm n $ BSL.toStrict $ rawStream sec n obj
  
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

findFontEncoding d sec os = findEncoding (fontObjs d os) sec os

findEncoding :: Dict -> Maybe Security -> PDFObjIndex -> [(String, Encoding)]
findEncoding dict sec objs = map pairwise dict
  where
    pairwise (PdfName n, ObjRef r) = (n, encoding sec r objs)
    pairwise x = ("", NullMap)

fontObjs :: Dict -> PDFObjIndex -> Dict
fontObjs dict objs = case findResourcesDict dict objs of
  Just d -> case findObjFromDict d "/Font" of
    Just (PdfDict d') -> d'
    Just (ObjRef x) -> case findDictByRef x objs of
                         Just d' -> d'
                         otherwise -> error "cannot find /Font dictionary"
    otherwise -> trace (show d) $ []
  Nothing -> []

findResourcesDict :: Dict -> PDFObjIndex -> Maybe Dict
findResourcesDict dict objs = case find resources dict of
  Just (_, ObjRef x)  -> findDictByRef x objs
  Just (_, PdfDict d) -> Just d
  otherwise -> error (show dict)
  where
    resources (PdfName "/Resources", _) = True
    resources _                         = False


encoding :: Maybe Security -> Int -> PDFObjIndex -> Encoding
encoding sec x objs = case subtype of
  Just (PdfName "/Type0") -> case encoding of
    Just (PdfName "/Identity-H") -> case cidSysInfo descendantFonts of
      (e:_) -> e
      []    -> NullMap
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
             CFF.encoding $ BSL.toStrict $ rawStreamByRef sec objs fontfile
           _ -> case findObjFromDict (fontDescriptor' x) "/FontFile" of
             Just (ObjRef fontfile) ->
               Type1.encoding $ BSL.toStrict $ rawStreamByRef sec objs fontfile
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
        Just (PdfArray dfrs : _) -> dfrs
        Just os -> case find isArray os of
          Just (PdfArray dfrs) -> dfrs
          _ -> []
        Nothing -> []
      _ -> []

    isArray (PdfArray _) = True
    isArray _            = False

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


findCMap :: Dict -> Maybe Security -> PDFObjIndex -> [(String, CMap)]
findCMap d sec objs = map pairwise (fontObjs d objs)
  where
    pairwise (PdfName n, ObjRef r) = (n, toUnicode sec r objs)
    pairwise x = ("", [])

toUnicode :: Maybe Security -> Int -> PDFObjIndex -> CMap
toUnicode sec x objs =
  case findObjFromDictWithRef x "/ToUnicode" objs of
    Just (ObjRef ref) ->
      let s = rawStreamByRef sec objs ref
      in if BSL.null s then noToUnicode sec x objs else parseCMap s
    otherwise -> noToUnicode sec x objs

noToUnicode :: Maybe Security -> Int -> PDFObjIndex -> CMap
noToUnicode sec x objs =
  case findObjFromDictWithRef x "/DescendantFonts" objs of
    Just (ObjRef ref) ->
      case findObjsByRef ref objs of
        Just [(PdfArray ((ObjRef subref):_))] ->
          case findObjFromDictWithRef subref "/FontDescriptor" objs of
            Just (ObjRef desc) ->
              case findObjFromDictWithRef desc "/FontFile2" objs of
                Just (ObjRef fontfile) ->
                  OpenType.cmap $ BSL.toStrict $ rawStreamByRef sec objs fontfile
                otherwise -> []
            otherwise -> []
        otherwise -> []
    otherwise -> []

