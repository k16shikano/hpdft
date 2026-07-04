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
import Control.Monad (replicateM, foldM)
import Codec.Compression.Zlib (decompress)
import qualified Data.Map as Map
import Data.Map (Map)

import PDF.Definition
import PDF.Object
import PDF.Error (PdfError(..), PdfResult, PdfWarning(..))
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
findObjs = collectPDFObjs

findObjs' :: BS.ByteString -> PdfResult [PDFBS]
findObjs' contents = case findTrailer' contents of
  Right (_, xref) ->
    Right $ Map.toAscList $ Map.map (extractObjBody contents) xref
  Left xrefErr -> case findObjs contents of
    [] -> Left xrefErr
    objs -> Right objs

extractObjBody :: BS.ByteString -> Int -> BS.ByteString
extractObjBody contents offset =
  case sliceObjectAt (BS.drop offset contents) of
    Just body -> body
    Nothing ->
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

contentsStream :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> PdfResult (PDFStream, [PdfWarning])
contentsStream dict st sec objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) -> getContentArray arr
  Just (PdfName "/Contents", ObjRef r) ->
    case findObjsByRef r objs of
      Just [PdfArray arr] -> getContentArray arr
      Just _ -> getContent r
      Nothing -> Left (MissingKey "/Contents" (show r))
  Nothing -> Left (MissingKey "/Contents" "page")
  where
    contents (PdfName "/Contents", _) = True
    contents _ = False

    getContentArray arr = parseContentStream dict st sec objs $
      BSL.concat [s | ref <- parseRefsArray arr
                     , Right s <- [rawStreamByRef sec objs ref]]
    getContent r = do
      s <- rawStreamByRef sec objs r
      parseContentStream dict st sec objs s

parseContentStream :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> BSL.ByteString -> PdfResult (PDFStream, [PdfWarning])
parseContentStream dict st sec objs s =
  parseStream (st {fontmaps=fontdict, cmaps=cmap}) s
  where fontdict = findFontEncoding dict sec objs
        cmap = findCMap dict sec objs

rawStreamByRef :: Maybe Security -> PDFObjIndex -> Int -> PdfResult BSL.ByteString
rawStreamByRef sec pdfobjs x = case findObjsByRef x pdfobjs of
  Just objs -> rawStream sec x objs
  Nothing   -> Left (ParseError "No object with stream to be shown" BS.empty)

rawStream :: Maybe Security -> Int -> [Obj] -> PdfResult BSL.ByteString
rawStream sec objNum objs = case find isStream objs of
  Just (PdfStream strm) -> rawStream' sec objNum (fromMaybe [] $ findDict objs) strm
  Nothing -> Left (ParseError "No object with stream to be shown"
                   (BS.take 80 $ BS.pack $ show objs))
  where
    isStream (PdfStream s) = True
    isStream _             = False

    rawStream' :: Maybe Security -> Int -> Dict -> BSL.ByteString -> PdfResult BSL.ByteString
    rawStream' sec' objNum' d s = do
      decoded <- decodeStreamBytes d $ BSL.fromStrict $
        decryptStream sec' objNum' 0 $ BSL.toStrict s
      return $ BSL.fromStrict decoded

decodeStreamBytes :: Dict -> BSL.ByteString -> PdfResult BS.ByteString
decodeStreamBytes d s = do
  filtered <- case find isFilter d of
    Just (PdfName "/Filter", PdfName "/FlateDecode") -> Right $ BSL.toStrict $ decompress s
    Nothing -> Right $ BSL.toStrict s
    Just (PdfName "/Filter", PdfName f) ->
      Left (UnsupportedFeature ("Unknown Stream Compression: " ++ f))
    Just _ -> Left (UnsupportedFeature "No Stream Compression Filter.")
  applyPredictor d filtered
  where
    isFilter (PdfName "/Filter", _) = True
    isFilter _                      = False

applyPredictor :: Dict -> BS.ByteString -> PdfResult BS.ByteString
applyPredictor d bs = case findObjFromDict d "/DecodeParms" of
  Just (PdfDict parms) ->
    case findObjFromDict parms "/Predictor" of
      Just (PdfNumber p) | truncate p >= 12 ->
        case findObjFromDict parms "/Columns" of
          Just (PdfNumber c) -> decodePNGPredictors bs (truncate c)
          _ -> Right bs
      _ -> Right bs
  _ -> Right bs

decodePNGPredictors :: BS.ByteString -> Int -> PdfResult BS.ByteString
decodePNGPredictors bs columns = go BS.empty bs
  where
    go _ rest | BS.null rest = Right BS.empty
    go prev rest = do
      let filt = BS.head rest
          enc  = BS.take columns (BS.drop 1 rest)
          rest' = BS.drop (1 + columns) rest
          prevRow = if BS.null prev then BS.replicate columns (chr 0) else prev
      row <- pngFilter filt enc prevRow
      more <- go row rest'
      return $ BS.append row more

pngFilter :: Char -> BS.ByteString -> BS.ByteString -> PdfResult BS.ByteString
pngFilter filt row prev
  | ord filt == 0 = Right row
  | ord filt == 1 = Right $ pngSub row
  | ord filt == 2 = Right $ pngUp row prev
  | otherwise =
      Left (UnsupportedFeature ("unsupported PNG predictor " ++ show (ord filt)))

pngUp :: BS.ByteString -> BS.ByteString -> BS.ByteString
pngUp row prev = BS.pack $ zipWith addByte (BS.unpack row) (BS.unpack prev)
  where addByte a b = chr ((ord a + ord b) `mod` 256)

pngSub :: BS.ByteString -> BS.ByteString
pngSub row =
  BS.pack $ snd $ foldl' (\(prev, out) x ->
    let n = (ord x + prev) `mod` 256 in (n, out ++ [chr n])) (0, []) (BS.unpack row)

contentsColorSpace :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> PdfResult [T.Text]
contentsColorSpace dict st sec objs = case find contents dict of
  Just (PdfName "/Contents", PdfArray arr) ->
    Right $ concat [cs | ref <- parseRefsArray arr
                        , Right cs <- [parseColorSpaceEntry ref]]
  Just (PdfName "/Contents", ObjRef x) ->
    parseColorSpaceEntry x
  Nothing -> Left (MissingKey "/Contents" "page")
  where
    contents (PdfName "/Contents", _) = True
    contents _                        = False
    xobjcs = findXObjectColorSpace dict objs
    parseColorSpaceEntry ref = do
      s <- rawStreamByRef sec objs ref
      parseColorSpace (st {xcolorspaces=xobjcs}) s


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

readDec' :: BS.ByteString -> PdfResult Int
readDec' bs = case readDec $ BS.unpack bs of
  [(n,_)] -> Right n
  _ -> Left (ParseError "invalid decimal" (BS.take 80 bs))

isPdfEofLine :: BS.ByteString -> Bool
isPdfEofLine line =
  case BS.dropWhile isSpaceChar line of
    rest | "%%EOF" `BS.isPrefixOf` rest ->
      BS.all isSpaceChar (BS.drop 5 rest)
    _ -> False

getStartxrefOffset :: BS.ByteString -> PdfResult Int
getStartxrefOffset source =
  let trimmed = BS.dropWhileEnd isSpaceChar source
      numLine = case BS.breakEnd (== '\n') trimmed of
        (_, n) | not (BS.null n) -> BS.dropWhile isSpaceChar n
        _ -> trimmed
  in readDec' $ BS.takeWhile (\c -> c >= '0' && c <= '9') numLine

isSpaceChar :: Char -> Bool
isSpaceChar c = c `elem` (" \t\r\n" :: String)

mergeXRefStm :: BS.ByteString -> Dict -> XREF -> PdfResult XREF
mergeXRefStm all dict xref = case findObjFromDict dict "/XRefStm" of
  Just (PdfNumber n) ->
    case findTrailerDictXREFStream $ BS.drop (truncate n) all of
      Right (_, stm) -> Right $ Map.union xref stm
      Left err -> Left err
  _ -> Right xref

findTrailer :: BS.ByteString -> PdfResult Dict
findTrailer bs = case BS.breakEnd (== '\n') bs of
  (source, eofLine)
    | isPdfEofLine eofLine -> do
        offset <- getStartxrefOffset source
        (dict, _) <- findTrailerDictXREF $ BS.drop offset bs
        return dict
    | source == "" -> Left (BrokenXref "no %%EOF or startxref found")
    | otherwise -> findTrailer (BS.init bs)

findTrailer' :: BS.ByteString -> PdfResult (Dict, XREF)
findTrailer' bs = case BS.breakEnd (== '\n') bs of
  (source, eofLine)
    | isPdfEofLine eofLine -> do
        offset <- getStartxrefOffset source
        (dict, xref) <- findTrailerDictXREF $ BS.drop offset bs
        xref' <- mergeXRefStm bs dict xref
        case find isPrev dict of
          Just (PdfName "/Prev", PdfNumber x) -> xrefs (truncate x) bs (dict, xref')
          Nothing -> Right (dict, xref')
    | source == "" -> Left (BrokenXref "no %%EOF or startxref found")
    | otherwise -> findTrailer' (BS.init bs)
  where
    isPrev (PdfName "/Prev", _) = True
    isPrev (_,_) = False

    xrefs n all (dict, sofar) = do
      (dict', xref) <- findTrailerDictXREF $ BS.drop n all
      xref' <- mergeXRefStm all dict' xref
      case find isPrev dict' of
        Just (PdfName "/Prev", PdfNumber x) ->
          xrefs (truncate x) all (dict, Map.union sofar xref')
        Nothing -> Right (dict, Map.union sofar xref')

findTrailerDictXREF :: BS.ByteString -> PdfResult (Dict, XREF)
findTrailerDictXREF xrefTrailer =
  let trimmed = BS.dropWhile isPdfSpace xrefTrailer
  in case BS.take 4 trimmed of
       "xref" -> findTrailerDictXREFTable xrefTrailer
       _      -> findTrailerDictXREFStream xrefTrailer

findTrailerDictXREFTable :: BS.ByteString -> PdfResult (Dict, XREF)
findTrailerDictXREFTable xrefTrailer = case BS.breakSubstring "trailer" xrefTrailer of
  (xref, trailer) | BS.null trailer ->
    Left (BrokenXref ("no trailer keyword: " ++ show (BS.take 80 xrefTrailer)))
  (xref, trailer) -> do
    dict <- case parseOnly (pdfdictionary <* spaces) (BS.drop 7 trailer) of
      Left err -> Left (ParseError ("trailer dictionary: " ++ show err) (BS.take 80 trailer))
      Right (PdfDict d) -> Right d
      Right _ -> Left (ParseError "expected trailer dictionary" (BS.take 80 trailer))
    xrefMap <- parseXref xref
    return (dict, xrefMap)

findTrailerDictXREFStream :: BS.ByteString -> PdfResult (Dict, XREF)
findTrailerDictXREFStream blob = case parseOnly xrefStreamObject blob of
  Left err ->
    Left (BrokenXref ("xref stream: " ++ show err ++ ": " ++ show (BS.take 80 blob)))
  Right (dict, s) -> do
    xrefMap <- xrefStreamToMap dict s
    return (dict, xrefMap)

parseXrefBlob :: BS.ByteString -> PdfResult XREF
parseXrefBlob bs =
  let trimmed = BS.dropWhile isPdfSpace bs
  in case BS.take 4 trimmed of
       "xref" -> parseXref bs
       _      -> parseXrefStream bs

parseXrefStream :: BS.ByteString -> PdfResult XREF
parseXrefStream bs = case parseOnly xrefStreamObject bs of
  Left err ->
    Left (BrokenXref ("xref stream: " ++ show err ++ ": " ++ show (BS.take 80 bs)))
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

xrefStreamToMap :: Dict -> BSL.ByteString -> PdfResult XREF
xrefStreamToMap dict s = do
  ws <- wFields dict
  sections <- indexSections dict
  raw <- decodeStreamBytes dict s
  entries <- xrefStreamEntries ws sections raw
  return $ Map.fromList [(n, off) | (n, typ, off) <- entries, typ == 1]

wFields :: Dict -> PdfResult (Int, Int, Int)
wFields d = case findObjFromDict d "/W" of
  Just (PdfArray [PdfNumber a, PdfNumber b, PdfNumber c]) ->
    Right (truncate a, truncate b, truncate c)
  _ -> Left (MissingKey "/W" "xref stream")

indexSections :: Dict -> PdfResult [(Int, Int)]
indexSections d = case findObjFromDict d "/Index" of
  Just (PdfArray arr) -> indexPairs arr
  Nothing -> case findObjFromDict d "/Size" of
    Just (PdfNumber s) -> Right [(0, truncate s)]
    _ -> Left (MissingKey "/Size" "xref stream")
  where
    indexPairs (PdfNumber a : PdfNumber b : xs) =
      ((truncate a, truncate b) :) <$> indexPairs xs
    indexPairs [] = Right []
    indexPairs _  = Left (BrokenXref "malformed /Index in xref stream")

xrefStreamEntries :: (Int, Int, Int) -> [(Int, Int)] -> BS.ByteString -> PdfResult [(Int, Int, Int)]
xrefStreamEntries widths sections raw =
  fst <$> foldM (parseSection widths) ([], raw) sections
  where
    parseSection (w0, w1, w2) (acc, bs) (start, count) = do
      (ents, rest) <- parseN (w0, w1, w2) start count bs
      return (acc ++ ents, rest)
    parseN (w0, w1, w2) start count bs =
      foldM (\(es, b) objNum -> do
                (typ, b1) <- readField w0 b
                (f2, b2)  <- readField w1 b1
                (_, b3)   <- readField w2 b2
                return ((objNum, typ, f2) : es, b3))
             ([], bs) [start .. start + count - 1]

readField :: Int -> BS.ByteString -> PdfResult (Int, BS.ByteString)
readField 0 bs = Right (0, bs)
readField w bs
  | BS.length bs < w =
      Left (BrokenXref ("xref stream field truncated: need "
                        ++ show w ++ " bytes, have " ++ show (BS.length bs)))
  | otherwise = Right (bytesToInt (BS.take w bs), BS.drop w bs)

bytesToInt :: BS.ByteString -> Int
bytesToInt = BS.foldl' (\acc w -> acc * 256 + ord w) 0

parseXref :: BS.ByteString -> PdfResult XREF
parseXref xref = case parseOnly xrefParser xref of
  Left err ->
    Left (BrokenXref ("xref table: " ++ show err ++ ": " ++ show (BS.take 80 xref)))
  Right xs -> do
    entries <- mapM concatSubsections xs
    return $ Map.fromList $ map dropFN $ filter (\(_,_,inUse) -> inUse) $ concat entries

  where
    xrefParser = do
      string "xref" >> spaces
      es <- (:) <$> subsections <*> many (try subsections)
      return es
    subsections = do
      begin <- takeWhile1 isDigit <* spaces
      num <- takeWhile1 isDigit <* spaces
      count <- parseDec num
      es <- replicateM count entries
      beginNum <- parseDec begin
      return (beginNum, count, es)
    entries = do
      offset <- P.take 10 <* spaces
      _gennum <- P.take 5 <* spaces
      status <- P.take 1 <* spaces
      _ <- optional (string "\r\n" <|> string "\n" <|> string "\r")
      off <- parseDec offset
      inUse <- forn status
      return (off, inUse)
    parseDec bs = case readDec $ BS.unpack bs of
      [(n,_)] -> return n
      _ -> fail ("invalid decimal: " ++ show (BS.take 80 bs))
    forn "f" = return False
    forn "n" = return True
    forn s = fail $ "xref entry status neither f nor n: " ++ show s

    concatSubsections (objn, 0, []) = Right []
    concatSubsections (objn, 1, e:[]) = Right [(objn, fst e, snd e)]
    concatSubsections (objn, 1, e:_) =
      Left (BrokenXref ("xref subsection count mismatch: " ++ show e))
    concatSubsections (objn, i, e:es) = do
      rest <- concatSubsections (objn+1, i-1, es)
      return ((objn, fst e, snd e) : rest)
    concatSubsections (_, _, e) =
      Left (BrokenXref ("xref subsection malformed: " ++ show e))

    dropFN (n, offset, fn) = (n, offset)

rootRef :: BS.ByteString -> PdfResult (Maybe Int)
rootRef bs = case findTrailer bs of
  Right dict -> Right $ findRefs isRootRef dict
  Left _     -> rootRefFromCRStream bs

rootRefFromCRStream :: BS.ByteString -> PdfResult (Maybe Int)
rootRefFromCRStream bs = do
  let ls = BS.lines bs
  case reverse ls of
    (_: numLine:_) -> do
      offset <- readDec' (BS.takeWhile (\c -> c >= '0' && c <= '9') numLine)
      objs <- findObjs' (BS.drop offset bs)
      case objs of
        ((_, crstrm):_) -> do
          (crdict, _) <- findTrailerDictXREF crstrm
          return $ findRefs isRootRef crdict
        [] -> Right Nothing
    _ -> Left (BrokenXref "cannot locate cross-reference stream offset")

isRootRef (PdfName "/Root", ObjRef x) = True
isRootRef (_,_) = False

findRefs :: ((Obj,Obj) -> Bool) -> Dict -> Maybe Int
findRefs pred dict = case find pred dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing


-- find Info

infoRef :: BS.ByteString -> PdfResult Int
infoRef bs = case findTrailer bs of
  Right dict -> case findRefs isInfoRef dict of
    Just r -> Right r
    Nothing -> Left (ParseError "No ref for info" BS.empty)
  Left err -> Left err

isInfoRef (PdfName "/Info", ObjRef x) = True
isInfoRef (_,_) = False


-- expand PDF 1.5 Object Stream 

expandObjStm :: Maybe Security -> [PDFObj] -> PdfResult [PDFObj]
expandObjStm sec os = concat <$> traverse (objStm sec) os

objStm :: Maybe Security -> PDFObj -> PdfResult [PDFObj]
objStm sec (n, obj) = case findDictOfType "/ObjStm" obj of
  Nothing -> Right [(n,obj)]
  Just _  -> do
    streamBytes <- rawStream sec n obj
    pdfObjStm n (BSL.toStrict streamBytes)
  
refOffset :: Parser ([(Int, Int)], String)
refOffset = spaces *> ((,) 
                       <$> many1 refPair
                       <*> many1 anyChar)
  where
    refPair = do
      rStr <- many1 digit <* spaces
      oStr <- many1 digit <* spaces
      case (readDec rStr, readDec oStr) of
        ([(r, "")], [(o, "")]) -> return (r, o)
        _ -> fail "invalid object stream reference"

pdfObjStm n s =
  case parseOnly refOffset s of
    Right (location, objstr) ->
      mapM (\(r,o) -> (,) r <$> parseDict (BS.pack $ drop o objstr)) location
    Left err ->
      Left (ParseError ("Failed to parse Object Stream: " ++ show err) (BS.take 80 s))
  where parseDict s' = case parseOnly pdfdictionary s' of
          Right obj -> Right [obj]
          Left  _   -> case parseOnly pdfarray s' of
            Right obj -> Right [obj]
            Left _ -> case parseOnly pdfletters s' of
              Right obj -> Right [obj]
              Left err ->
                Left (ParseError ((show err) ++ ": Failed to parse obj around")
                      (BS.take 100 s'))


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
                         _       -> []
    _ -> []
  Nothing -> []

findResourcesDict :: Dict -> PDFObjIndex -> Maybe Dict
findResourcesDict dict objs = case find resources dict of
  Just (_, ObjRef x)  -> findDictByRef x objs
  Just (_, PdfDict d) -> Just d
  _ -> Nothing
  where
    resources (PdfName "/Resources", _) = True
    resources _                         = False


encoding :: Maybe Security -> Int -> PDFObjIndex -> Encoding
encoding sec x objs = case subtype of
  Just (PdfName "/Type0") -> case encoding of
    Just (PdfName "/Identity-H") -> case cidSysInfo descendantFonts of
      (e:_) -> e
      []    -> NullMap
    Just (PdfName _) -> NullMap
    _ -> NullMap
  Just (PdfName "/Type1") -> case encoding of
    Just (ObjRef r) -> case findObjFromDictWithRef r "/Differences" objs of
                     Just (PdfArray arr) -> charDiff arr
                     _ -> NullMap
    Just (PdfDict d) -> case findObjFromDict d "/Differences" of
                     Just (PdfArray arr) -> charDiff arr
                     _ -> NullMap
    Just (PdfName "/MacRomanEncoding") -> NullMap
    Just (PdfName "/MacExpertEncoding") -> NullMap
    Just (PdfName "/WinAnsiEncoding") -> NullMap
    -- TODO: FontFile (Type 1), FontFile2 (TrueType), FontFile3 (Other than Type1C)
    _ -> case findObjFromDict (fontDescriptor' x) "/FontFile3" of
           Just (ObjRef fontfile) ->
             case rawStreamByRef sec objs fontfile of
               Right bs -> CFF.encoding $ BSL.toStrict bs
               Left _ -> NullMap
           _ -> case findObjFromDict (fontDescriptor' x) "/FontFile" of
             Just (ObjRef fontfile) ->
               case rawStreamByRef sec objs fontfile of
                 Right bs -> Type1.encoding $ BSL.toStrict bs
                 Left _ -> NullMap
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
                           _ -> WithCharSet ""
      _ -> WithCharSet ""

    fontDescriptor :: [Obj] -> [Dict]
    fontDescriptor [] = []
    fontDescriptor ((ObjRef r):rs) = (fontDescriptor' r):(fontDescriptor rs)
    -- Base-14 fonts legally omit /FontDescriptor; treat as empty dict.
    fontDescriptor' :: Int -> Dict
    fontDescriptor' fdr = case findObjFromDictWithRef fdr "/FontDescriptor" objs of
      Just (ObjRef r) -> fromMaybe [] $ findDictByRef r objs
      Just (PdfDict d) -> d
      _ -> []

    getCIDSystemInfo d =
      let registry = case findObjFromDict d "/Registry" of
                       Just (PdfText r) -> r
                       _ -> ""
          ordering = case findObjFromDict d "/Ordering" of
                       Just (PdfText o) -> o
                       _ -> ""
          cmap = registry ++ "-" ++ ordering
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
        charmap (_:xs) i = charmap xs i
        charmap [] _               = []
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
      case rawStreamByRef sec objs ref of
        Right s | BSL.null s -> noToUnicode sec x objs
                | otherwise  -> parseCMap s
        _ -> noToUnicode sec x objs
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
                  case rawStreamByRef sec objs fontfile of
                    Right bs -> OpenType.cmap $ BSL.toStrict bs
                    Left _ -> []
                otherwise -> []
            otherwise -> []
        otherwise -> []
    otherwise -> []

