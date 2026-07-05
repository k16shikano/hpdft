{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.DocumentStructure
Description : Function to walk around Document Structure of a PDF file
Copyright   : (c) Keiichiro Shikano, 2020
License     : MIT
Maintainer  : k16.shikano@gmail.com
-}

module PDF.DocumentStructure
       ( buildIndex
       , buildIndexEager
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
       , findRefs
       , findObjsByRef
       , findObjs
       , findObjs'
       , findTrailer
       , findTrailer'
       , indexPDFObjs
       , extractObjBody
       , rawStream
       , fontInfo
       , parseCIDWidths
       , simpleWidthAt
       , findResourcesDict
       ) where

import Data.Char (chr, isDigit, ord)
import Data.List (find, foldl', isSuffixOf, nub)
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
import qualified Data.Map as M
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
    Right
      [ (n, extractObjBody contents off)
      | (n, InFile off) <- M.toAscList xref
      ]
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
findObjsByRef = M.lookup

indexPDFObjs :: [PDFObj] -> PDFObjIndex
indexPDFObjs = M.fromList

buildIndex :: BS.ByteString -> Maybe Security -> XREF -> PDFObjIndex
buildIndex bytes msec xref = objs
  where
    objs = M.mapWithKey resolve xref
    containerCache =
      M.fromList
        [ (cnum, objStmEntries cnum)
        | cnum <- nub [c | InObjStm c _ <- M.elems xref]
        ]
    resolve objNum (InFile off) =
      let body = extractObjBody bytes off
      in snd (parsePDFObj msec (objNum, body))
    resolve objNum (InObjStm cnum idx) =
      case M.lookup cnum containerCache of
        Just entries ->
          case lookup objNum entries of
            Just o -> o
            Nothing ->
              case drop idx entries of
                ((_, o) : _) -> o
                _ -> [PdfNull]
        Nothing -> [PdfNull]
    objStmEntries cnum =
      case rawStream msec cnum (M.findWithDefault [PdfNull] cnum objs) of
        Right streamBytes ->
          case pdfObjStm cnum (BSL.toStrict streamBytes) of
            Right entries -> entries
            Left _ -> []
        Left _ -> []

buildIndexEager :: BS.ByteString -> Maybe Security -> PdfResult PDFObjIndex
buildIndexEager bytes msec = do
  rawObjs <- case findObjs bytes of
    [] -> Left (BrokenXref "no objects found without xref")
    objs -> Right objs
  let parsed = map (parsePDFObj msec) rawObjs
  expanded <- expandObjStm msec parsed
  return (indexPDFObjs expanded)

findObjFromDictWithRef :: Int -> String -> PDFObjIndex -> Maybe Obj
findObjFromDictWithRef ref name objs = case findDictByRef ref objs of 
  Just d -> findObjFromDict d name
  Nothing -> Nothing
  
findObjFromDict :: Dict -> String -> Maybe Obj
findObjFromDict d name = M.lookup name d

findDictByRef :: Int -> PDFObjIndex -> Maybe Dict
findDictByRef ref objs = case findObjsByRef ref objs of
  Just os -> findDict os
  Nothing -> Nothing

findDictOfType :: String -> [Obj] -> Maybe Dict
findDictOfType typename objs = case findDict objs of
  Just d  -> if isType d then Just d else Nothing 
  Nothing -> Nothing
  where 
    isType dict = M.lookup "/Type" dict == Just (PdfName typename)
 
findDict :: [Obj] -> Maybe Dict
findDict objs = case find isDict objs of
  Just (PdfDict d) -> Just d
  otherwise -> Nothing
  where 
    isDict :: Obj -> Bool
    isDict (PdfDict d) = True
    isDict _           = False

findPages :: Dict -> Maybe Int
findPages dict = case M.lookup "/Pages" dict of
  Just (ObjRef x) -> Just x
  _               -> Nothing
    
findKids :: Dict -> Maybe [Int]
findKids dict = case M.lookup "/Kids" dict of
  Just (PdfArray arr) -> Just (parseRefsArray arr)
  _                   -> Nothing

contentsStream :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> PdfResult (PDFStream, [PdfWarning])
contentsStream dict st sec objs = case M.lookup "/Contents" dict of
  Just (PdfArray arr) -> getContentArray arr
  Just (ObjRef r) ->
    case findObjsByRef r objs of
      Just [PdfArray arr] -> getContentArray arr
      Just _ -> getContent r
      Nothing -> Left (MissingKey "/Contents" (show r))
  Nothing -> Left (MissingKey "/Contents" "page")
  where
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
  Just (PdfStream strm) -> rawStream' sec objNum (fromMaybe M.empty $ findDict objs) strm
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
  filtered <- case M.lookup "/Filter" d of
    Just (PdfName "/FlateDecode") -> Right $ BSL.toStrict $ decompress s
    Nothing -> Right $ BSL.toStrict s
    Just (PdfName f) ->
      Left (UnsupportedFeature ("Unknown Stream Compression: " ++ f))
    Just _ -> Left (UnsupportedFeature "No Stream Compression Filter.")
  applyPredictor d filtered

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
contentsColorSpace dict st sec objs = case M.lookup "/Contents" dict of
  Just (PdfArray arr) ->
    Right $ concat [cs | ref <- parseRefsArray arr
                        , Right cs <- [parseColorSpaceEntry ref]]
  Just (ObjRef x) ->
    parseColorSpaceEntry x
  Nothing -> Left (MissingKey "/Contents" "page")
  where
    xobjcs = findXObjectColorSpace dict objs
    parseColorSpaceEntry ref = do
      s <- rawStreamByRef sec objs ref
      parseColorSpace (st {xcolorspaces=xobjcs}) s


-- find XObject

findXObjectColorSpace d os = xobjColorSpaceMap (findXObject d os) os

xobjColorSpaceMap dict objs =
  [ xobjColorSpace r objs | (_, ObjRef r) <- M.toList dict ]

findXObject dict objs = case findResourcesDict dict objs of
  Just d -> case findObjFromDict d "/XObject" of
    Just (PdfDict d) -> d
    otherwise -> M.empty
  Nothing -> M.empty

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
      Right (_, stm) -> Right $ M.union xref stm
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
        case M.lookup "/Prev" dict of
          Just (PdfNumber x) -> xrefs (truncate x) bs (dict, xref')
          _ -> Right (dict, xref')
    | source == "" -> Left (BrokenXref "no %%EOF or startxref found")
    | otherwise -> findTrailer' (BS.init bs)
  where
    xrefs n all (dict, sofar) = do
      (dict', xref) <- findTrailerDictXREF $ BS.drop n all
      xref' <- mergeXRefStm all dict' xref
      case M.lookup "/Prev" dict' of
        Just (PdfNumber x) ->
          xrefs (truncate x) all (dict, M.union sofar xref')
        _ -> Right (dict, M.union sofar xref')

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
  return $ M.fromList
    [ (n, entry)
    | (n, typ, f2, f3) <- entries
    , Just entry <- [xrefEntry typ f2 f3]
    ]
  where
    xrefEntry 1 off _ = Just (InFile off)
    xrefEntry 2 cnum idx = Just (InObjStm cnum idx)
    xrefEntry 0 _ _ = Nothing
    xrefEntry _ _ _ = Nothing

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

xrefStreamEntries :: (Int, Int, Int) -> [(Int, Int)] -> BS.ByteString -> PdfResult [(Int, Int, Int, Int)]
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
                (f3, b3)  <- readField w2 b2
                return ((objNum, typ, f2, f3) : es, b3))
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
    return $ M.fromList $ map dropFN $ filter (\(_,_,inUse) -> inUse) $ concat entries

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

    dropFN (n, offset, fn) = (n, InFile offset)

rootRef :: BS.ByteString -> PdfResult (Maybe Int)
rootRef bs = case findTrailer bs of
  Right dict -> Right $ findRefs "/Root" dict
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
          return $ findRefs "/Root" crdict
        [] -> Right Nothing
    _ -> Left (BrokenXref "cannot locate cross-reference stream offset")

findRefs :: String -> Dict -> Maybe Int
findRefs key dict = case M.lookup key dict of
  Just (ObjRef x) -> Just x
  _               -> Nothing


-- find Info

infoRef :: BS.ByteString -> PdfResult Int
infoRef bs = case findTrailer bs of
  Right dict -> case findRefs "/Info" dict of
    Just r -> Right r
    Nothing -> Left (ParseError "No ref for info" BS.empty)
  Left err -> Left err

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

findEncoding :: Dict -> Maybe Security -> PDFObjIndex -> Map String Encoding
findEncoding dict sec objs = M.fromListWith (flip const) $
  [ (n, encoding sec r objs) | (n, ObjRef r) <- M.toList dict ]

fontObjs :: Dict -> PDFObjIndex -> Dict
fontObjs dict objs = case findResourcesDict dict objs of
  Just d -> case findObjFromDict d "/Font" of
    Just (PdfDict d') -> d'
    Just (ObjRef x) -> case findDictByRef x objs of
                         Just d' -> d'
                         _       -> M.empty
    _ -> M.empty
  Nothing -> M.empty

findResourcesDict :: Dict -> PDFObjIndex -> Maybe Dict
findResourcesDict dict objs = case M.lookup "/Resources" dict of
  Just (ObjRef x)  -> findDictByRef x objs
  Just (PdfDict d) -> Just d
  _ -> Nothing


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
      Just (ObjRef r) -> fromMaybe M.empty $ findDictByRef r objs
      Just (PdfDict d) -> d
      _ -> M.empty

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
charDiff objs = Encoding $ M.fromListWith (flip const) $ charmap objs 0
  where charmap (PdfNumber x : PdfName n : xs) i = 
          if i < truncate x then 
            (chr $ truncate x, n) : (charmap xs $ incr x)
          else 
            (chr $ i, n) : (charmap xs $ i+1)
        charmap (PdfName n : xs) i = (chr i, n) : (charmap xs $ i+1)
        charmap (_:xs) i = charmap xs i
        charmap [] _               = []
        incr x = (truncate x) + 1


findCMap :: Dict -> Maybe Security -> PDFObjIndex -> Map String CMap
findCMap d sec objs = M.fromListWith (flip const) $
  [ (n, toUnicode sec r objs) | (n, ObjRef r) <- M.toList (fontObjs d objs) ]

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
                    Left _ -> M.empty
                otherwise -> M.empty
            otherwise -> M.empty
        otherwise -> M.empty
    otherwise -> M.empty


fontInfo :: Maybe Security -> Int -> PDFObjIndex -> FontInfo
fontInfo sec x objs =
  case findObjFromDictWithRef x "/Subtype" objs of
    Just (PdfName "/Type0") -> type0FontInfo sec x objs
    _ -> simpleFontInfo sec x objs

simpleFontInfo :: Maybe Security -> Int -> PDFObjIndex -> FontInfo
simpleFontInfo sec x objs =
  let enc = encoding sec x objs
      tuc = toUnicode sec x objs
      fd = fontDescriptorFor x objs
      defaultW = case findObjFromDict fd "/MissingWidth" of
        Just (PdfNumber w) -> w
        _ -> 0
      firstChar = case findObjFromDictWithRef x "/FirstChar" objs of
        Just (PdfNumber n) -> truncate n
        _ -> 0
      widths = case findObjFromDictWithRef x "/Widths" objs of
        Just wobj -> resolveObjArray wobj objs
        _ -> []
      widthFn code = simpleWidthAt firstChar widths defaultW code
  in FontInfo
    { fiEncoding = enc
    , fiToUnicode = tuc
    , fiWidth = widthFn
    , fiWidthV = const defaultVerticalW1
    , fiWMode = 0
    , fiBytesPerCode = 1
    , fiDefaultWidth = defaultW
    }

type0FontInfo :: Maybe Security -> Int -> PDFObjIndex -> FontInfo
type0FontInfo sec x objs =
  let enc = encoding sec x objs
      tuc = toUnicode sec x objs
      cidDict = firstDescendantFont x objs
      defaultW = case cidDict >>= \d -> findObjFromDict d "/DW" of
        Just (PdfNumber w) -> w
        _ -> 1000
      widthMap = case cidDict >>= \d -> findObjFromDict d "/W" of
        Just wobj -> parseCIDWidths (resolveObjArray wobj objs)
        _ -> M.empty
      (_, w1Default) = dw2Defaults cidDict
      widthVMap = case cidDict >>= \d -> findObjFromDict d "/W2" of
        Just wobj -> parseCIDVerticalWidths (resolveObjArray wobj objs)
        _ -> M.empty
      wmode = wmodeFromEncoding (findObjFromDictWithRef x "/Encoding" objs)
      widthFn cid = M.findWithDefault defaultW cid widthMap
      widthVFn cid = M.findWithDefault w1Default cid widthVMap
  in FontInfo
    { fiEncoding = enc
    , fiToUnicode = tuc
    , fiWidth = widthFn
    , fiWidthV = widthVFn
    , fiWMode = wmode
    , fiBytesPerCode = 2
    , fiDefaultWidth = defaultW
    }

defaultVerticalW1 :: Double
defaultVerticalW1 = -1000

fontDescriptorFor :: Int -> PDFObjIndex -> Dict
fontDescriptorFor fdr objs = case findObjFromDictWithRef fdr "/FontDescriptor" objs of
  Just (ObjRef r) -> fromMaybe M.empty $ findDictByRef r objs
  Just (PdfDict d) -> d
  _ -> M.empty

resolveObjArray :: Obj -> PDFObjIndex -> [Obj]
resolveObjArray obj objs = case obj of
  ObjRef r -> case findObjsByRef r objs of
    Just [PdfArray arr] -> arr
    Just os -> maybe [] id (findFirstArray os)
    Nothing -> []
  PdfArray arr -> arr
  _ -> []

findFirstArray :: [Obj] -> Maybe [Obj]
findFirstArray os = case find isArrayObj os of
  Just (PdfArray arr) -> Just arr
  _ -> Nothing
  where
    isArrayObj (PdfArray _) = True
    isArrayObj _            = False

firstDescendantFont :: Int -> PDFObjIndex -> Maybe Dict
firstDescendantFont fontRef objs =
  case descendantFontObjs fontRef objs of
    (df:_) -> cidFontDict df objs
    _ -> Nothing

descendantFontObjs :: Int -> PDFObjIndex -> [Obj]
descendantFontObjs fontRef objs =
  case findObjFromDictWithRef fontRef "/DescendantFonts" objs of
    Just (PdfArray dfrs) -> dfrs
    Just (ObjRef r) -> case findObjsByRef r objs of
      Just (PdfArray dfrs : _) -> dfrs
      Just os -> fromMaybe [] (findFirstArray os)
      Nothing -> []
    _ -> []

cidFontDict :: Obj -> PDFObjIndex -> Maybe Dict
cidFontDict df objs = case df of
  ObjRef r -> findDictByRef r objs
  PdfDict d -> Just d
  _ -> Nothing

dw2Defaults :: Maybe Dict -> (Double, Double)
dw2Defaults Nothing = (880, defaultVerticalW1)
dw2Defaults (Just d) = case findObjFromDict d "/DW2" of
  Just (PdfArray [PdfNumber vy, PdfNumber w1]) -> (vy, w1)
  _ -> (880, defaultVerticalW1)

wmodeFromEncoding :: Maybe Obj -> Int
wmodeFromEncoding (Just (PdfName n)) | "-V" `isSuffixOf` n = 1
wmodeFromEncoding _ = 0

simpleWidthAt :: Int -> [Obj] -> Double -> Int -> Double
simpleWidthAt firstChar widths defaultWidth code =
  let idx = code - firstChar
  in if idx >= 0 && idx < length widths
     then case widths !! idx of
            PdfNumber w -> w
            _ -> defaultWidth
     else defaultWidth

parseCIDWidths :: [Obj] -> Map Int Double
parseCIDWidths = foldCIDMetrics widthFromObj M.empty

parseCIDVerticalWidths :: [Obj] -> Map Int Double
parseCIDVerticalWidths = foldCIDMetrics verticalW1FromObj M.empty

foldCIDMetrics :: (Obj -> Maybe Double) -> Map Int Double -> [Obj] -> Map Int Double
foldCIDMetrics metricFn = go
  where
    go m [] = m
    go m (PdfNumber c : PdfArray ws : rest) =
      let m' = foldl' insertAt m (zip [0..] ws)
          insertAt acc (i, w) = case metricFn w of
            Just n -> M.insert (truncate c + i) n acc
            Nothing -> acc
      in go m' rest
    go m (PdfNumber cFirst : PdfNumber cLast : w : rest) =
      case metricFn w of
        Just n ->
          let m' = foldl' (\acc cid -> M.insert cid n acc) m [truncate cFirst .. truncate cLast]
          in go m' rest
        Nothing -> go m rest
    go m (_:rest) = go m rest

widthFromObj :: Obj -> Maybe Double
widthFromObj (PdfNumber w) = Just w
widthFromObj _ = Nothing

verticalW1FromObj :: Obj -> Maybe Double
verticalW1FromObj (PdfArray [PdfNumber _vx, PdfNumber vy]) = Just vy
verticalW1FromObj (PdfNumber w) = Just w
verticalW1FromObj _ = Nothing

