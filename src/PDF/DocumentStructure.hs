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
       , fontInfoFromDict
       , parseCIDWidths
       , simpleWidthAt
       , findResourcesDict
       , decodeStreamBytes
       , streamFilterNames
       ) where

import Data.Char (chr, isDigit, ord)
import Data.List (find, foldl', isSuffixOf, nub)
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Data.Maybe (fromMaybe, listToMaybe)
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
        [ (cnum, objStmBody cnum)
        | cnum <- nub [c | InObjStm c _ <- M.elems xref]
        ]
    resolve objNum (InFile off) =
      let body = extractObjBody bytes off
      in snd (parsePDFObj msec (objNum, body))
    resolve objNum (InObjStm cnum idx) =
      case M.lookup cnum containerCache of
        Just (locations, body) ->
          let off = case listToMaybe (drop idx locations) of
                Just (_, o) -> Just o
                Nothing -> lookup objNum locations
          in maybe [PdfNull] (parseObjStmObject body) off
        Nothing -> [PdfNull]
    objStmBody cnum =
      let containerObjs = M.findWithDefault [PdfNull] cnum objs
      in case rawStream msec cnum containerObjs of
        Right streamBytes ->
          case parseObjStmHeader (objStmFirst containerObjs) (BSL.toStrict streamBytes) of
            Right cache -> cache
            Left _ -> ([], BS.empty)
        Left _ -> ([], BS.empty)

buildIndexEager :: BS.ByteString -> Maybe Security -> PdfResult PDFObjIndex
buildIndexEager bytes msec = do
  rawObjs <- case findObjs bytes of
    [] -> Left (BrokenXref "no objects found without xref")
    objs -> Right objs
  let parsed = map (parsePDFObj msec) rawObjs
  expanded <- expandObjStm msec parsed
  return (indexPDFObjs expanded)

findObjFromDictWithRef :: Int -> T.Text -> PDFObjIndex -> Maybe Obj
findObjFromDictWithRef ref name objs = case findDictByRef ref objs of 
  Just d -> findObjFromDict d name
  Nothing -> Nothing
  
findObjFromDict :: Dict -> T.Text -> Maybe Obj
findObjFromDict d name = M.lookup name d

findDictByRef :: Int -> PDFObjIndex -> Maybe Dict
findDictByRef ref objs = case findObjsByRef ref objs of
  Just os -> findDict os
  Nothing -> Nothing

findDictOfType :: T.Text -> [Obj] -> Maybe Dict
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
  parseStream (formTextRunner sec objs) (st {fontmaps=fontdict, cmaps=cmap, psResDict=Just dict}) s
  where fontdict = findFontEncoding dict sec objs
        cmap = findCMap dict sec objs

maxLegacyFormDepth :: Int
maxLegacyFormDepth = 12

formTextRunner :: Maybe Security -> PDFObjIndex -> T.Text -> PSR -> T.Text
formTextRunner sec objs name st
  | psFormDepth st >= maxLegacyFormDepth = T.empty
  | otherwise =
      case formStream sec objs (psResDict st) name of
        Nothing -> T.empty
        Just (formDict, stream) ->
          let runner = formTextRunner sec objs
              st' = st { fontmaps = M.union (findFontEncoding formDict sec objs) (fontmaps st)
                       , cmaps = M.union (findCMap formDict sec objs) (cmaps st)
                       , psResDict = Just formDict
                       , psFormDepth = psFormDepth st + 1
                       }
          in case parseStream runner st' stream of
               Right (txt, _) -> T.pack (BSLU.toString txt)
               Left _ -> T.empty

formStream :: Maybe Security -> PDFObjIndex -> Maybe Dict -> T.Text -> Maybe (Dict, PDFStream)
formStream sec objs resDict name =
  case resDict >>= xobjectDict objs >>= M.lookup name of
    Just (ObjRef r) ->
      case findDictByRef r objs of
        Just d | M.lookup "/Subtype" d == Just (PdfName "/Form") ->
          case rawStreamByRef sec objs r of
            Right stream -> Just (d, stream)
            Left _ -> Nothing
        _ -> Nothing
    _ -> Nothing

xobjectDict :: PDFObjIndex -> Dict -> Maybe (M.Map T.Text Obj)
xobjectDict objs d =
  case findResourcesDict d objs of
    Just rd -> case findObjFromDict rd "/XObject" of
      Just (PdfDict xd) -> Just xd
      Just (ObjRef xr) -> findDictByRef xr objs
      _ -> Nothing
    Nothing -> Nothing

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

supportedStreamFilters :: [String]
supportedStreamFilters =
  [ "/FlateDecode", "/DCTDecode", "/ASCII85Decode" ]

decodeStreamBytes :: Dict -> BSL.ByteString -> PdfResult BS.ByteString
decodeStreamBytes d s = do
  filters <- streamFilterNames d
  filtered <- applyStreamFilters filters s
  applyPredictor d filtered

streamFilterNames :: Dict -> PdfResult [T.Text]
streamFilterNames d = case M.lookup "/Filter" d of
  Nothing -> Right []
  Just (PdfName n) -> Right [n]
  Just (PdfArray arr) ->
    Right [ n | PdfName n <- arr ]
  Just _ ->
    Left (UnsupportedFeature "invalid /Filter entry (expected name or array of names)")

applyStreamFilters :: [T.Text] -> BSL.ByteString -> PdfResult BS.ByteString
applyStreamFilters [] s = Right (BSL.toStrict s)
applyStreamFilters (f : fs) s = do
  step <- decodeOneStreamFilter f s
  applyStreamFilters fs (BSL.fromStrict step)

decodeOneStreamFilter :: T.Text -> BSL.ByteString -> PdfResult BS.ByteString
decodeOneStreamFilter "/FlateDecode" s =
  Right $ BSL.toStrict $ decompress s
decodeOneStreamFilter "/DCTDecode" s =
  Right $ BSL.toStrict s
decodeOneStreamFilter "/ASCII85Decode" s =
  decodeASCII85 (BSL.toStrict s)
decodeOneStreamFilter f _ =
  Left $
    UnsupportedFeature
      ( "unsupported stream filter "
          ++ T.unpack f
          ++ " (supported: "
          ++ unwords supportedStreamFilters
          ++ ")"
      )

-- | PDF ASCII85Decode (base-85, 'z' = four zero bytes, '~>' EOD).
decodeASCII85 :: BS.ByteString -> PdfResult BS.ByteString
decodeASCII85 bs =
  Right $ BS.pack $ go (Prelude.filter isAscii85Data (BS.unpack bs)) []
  where
    isAscii85Data c =
      let o = ord c
       in (o >= 33 && o <= 117) || c == 'z' || c == 'Z'
    go [] acc = reverse acc
    go ('z' : rest) acc = go rest ('\0' : '\0' : '\0' : '\0' : acc)
    go ('Z' : rest) acc = go rest ('\0' : '\0' : '\0' : '\0' : acc)
    go cs acc =
      let grp = Prelude.take 5 cs
          rest = drop 5 cs
          pad = replicate (5 - Prelude.length grp) 'u'
          vals = map (\c -> ord c - 33) (grp ++ pad)
          n = foldl' (\a x -> a * 85 + x) 0 vals
          bytes =
            [ (n `shiftR` 24) .&. 0xff
            , (n `shiftR` 16) .&. 0xff
            , (n `shiftR` 8) .&. 0xff
            , n .&. 0xff
            ]
          out = map chr (Prelude.take (max 0 (Prelude.length grp - 1)) (reverse bytes))
       in if null grp then reverse acc else go rest (out ++ acc)

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

xobjColorSpace :: Int -> PDFObjIndex -> T.Text
xobjColorSpace x objs = case findObjFromDictWithRef x "/ColorSpace" objs of
  Just (PdfName cs) -> cs
  otherwise -> T.empty


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

isEol :: Char -> Bool
isEol c = c == '\n' || c == '\r'

splitLastLine :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitLastLine bs =
  let trimmed = BS.dropWhileEnd isEol bs
      rev = BS.reverse trimmed
      (lineRev, restRev) = BS.break isEol rev
  in ( BS.reverse restRev
     , BS.reverse lineRev
     )

getStartxrefOffset :: BS.ByteString -> PdfResult Int
getStartxrefOffset source =
  let trimmed = BS.dropWhileEnd isSpaceChar source
      (_, numLine) = splitLastLine trimmed
  in readDec' $ BS.takeWhile (\c -> c >= '0' && c <= '9') (BS.dropWhile isSpaceChar numLine)

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
findTrailer bs = case splitLastLine bs of
  (source, eofLine)
    | isPdfEofLine eofLine -> do
        offset <- getStartxrefOffset source
        (dict, _) <- findTrailerDictXREF $ BS.drop offset bs
        return dict
    | source == "" -> Left (BrokenXref "no %%EOF or startxref found")
    | otherwise -> findTrailer source

findTrailer' :: BS.ByteString -> PdfResult (Dict, XREF)
findTrailer' bs = case splitLastLine bs of
  (source, eofLine)
    | isPdfEofLine eofLine -> do
        offset <- getStartxrefOffset source
        (dict, xref) <- findTrailerDictXREF $ BS.drop offset bs
        xref' <- mergeXRefStm bs dict xref
        case M.lookup "/Prev" dict of
          Just (PdfNumber x) -> xrefs (truncate x) bs (dict, xref')
          _ -> Right (dict, xref')
    | source == "" -> Left (BrokenXref "no %%EOF or startxref found")
    | otherwise -> findTrailer' source
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

findRefs :: T.Text -> Dict -> Maybe Int
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
  Just d  -> do
    streamBytes <- rawStream sec n obj
    pdfObjStm n (objStmFirstFromDict d) (BSL.toStrict streamBytes)

objStmFirstFromDict :: Dict -> Maybe Int
objStmFirstFromDict d = case findObjFromDict d "/First" of
  Just (PdfNumber n) -> Just (truncate n)
  _ -> Nothing

objStmFirst :: [Obj] -> Maybe Int
objStmFirst objs = findDict objs >>= objStmFirstFromDict

refPair :: Parser (Int, Int)
refPair = do
  rStr <- many1 digit <* spaces
  oStr <- many1 digit <* spaces
  case (readDec rStr, readDec oStr) of
    ([(r, "")], [(o, "")]) -> return (r, o)
    _ -> fail "invalid object stream reference"

refPairs :: Parser [(Int, Int)]
refPairs = spaces *> many1 refPair

refOffset :: Parser ([(Int, Int)], BS.ByteString)
refOffset = do
  location <- refPairs
  rest <- BS.pack <$> many1 anyChar
  return (location, rest)

pdfObjStm :: Int -> Maybe Int -> BS.ByteString -> PdfResult [PDFObj]
pdfObjStm n mFirst s =
  case parseObjStmHeader mFirst s of
    Right (location, body) ->
      Right [ (r, parseObjStmObject body o) | (r, o) <- location ]
    Left err ->
      Left (ParseError ("Failed to parse Object Stream: " ++ show err) (BS.take 80 s))

parseObjStmHeader :: Maybe Int -> BS.ByteString -> Either String ([(Int, Int)], BS.ByteString)
parseObjStmHeader (Just first) s
  | first >= 0 && first <= BS.length s =
      case parseOnly refPairs (BS.take first s) of
        Right location -> Right (location, BS.drop first s)
        Left err -> Left (show err)
parseObjStmHeader _ s =
  case parseOnly refOffset s of
    Right (location, body) -> Right (location, body)
    Left err -> Left (show err)

parseObjStmObject :: BS.ByteString -> Int -> [Obj]
parseObjStmObject body off =
  case parseObjStmValue (BS.drop off body) of
    Right obj -> obj
    Left _ -> [PdfNull]

parseObjStmValue :: BS.ByteString -> Either String [Obj]
parseObjStmValue s' = case parseOnly pdfdictionary s' of
  Right obj -> Right [obj]
  Left _ -> case parseOnly pdfarray s' of
    Right obj -> Right [obj]
    Left _ -> case parseOnly pdfletters s' of
      Right obj -> Right [obj]
      Left err -> Left (show err)


-- make fontmap from page's /Resources (see 3.7.2 of PDF Ref.)

findFontEncoding d sec os = findEncoding (fontObjs d os) sec os

findEncoding :: Dict -> Maybe Security -> PDFObjIndex -> Map T.Text Encoding
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
encoding sec x objs =
  case findDictByRef x objs of
    Just d -> encodingFromDict sec objs d
    Nothing -> NullMap

encodingFromDict :: Maybe Security -> PDFObjIndex -> Dict -> Encoding
encodingFromDict sec objs d = case subtype of
  Just (PdfName "/Type0") -> case encField of
    Just (PdfName "/Identity-H") -> case cidSysInfo descendantFonts of
      (e:_) -> e
      []    -> NullMap
    Just (PdfName n) | n `elem` sjisEncodings -> SJISmap
    Just (PdfName n) | n `elem` unijisEncodings -> UnicodeMap
    Just (PdfName "/H") -> JISmap
    Just (PdfName "/V") -> JISmap
    Just (PdfName _) -> NullMap
    _ -> NullMap
  Just (PdfName "/Type1") -> case encField of
    Just (ObjRef r) -> case findObjFromDictWithRef r "/Differences" objs of
                     Just (PdfArray arr) -> charDiff arr
                     _ -> NullMap
    Just (PdfDict ed) -> case findObjFromDict ed "/Differences" of
                     Just (PdfArray arr) -> charDiff arr
                     _ -> NullMap
    Just (PdfName "/MacRomanEncoding") -> NullMap
    Just (PdfName "/MacExpertEncoding") -> NullMap
    Just (PdfName "/WinAnsiEncoding") -> NullMap
    Just (PdfName "/ZapfDingbats") -> WithCharSet "ZapfDingbats"
    Just (PdfName "/Symbol") -> WithCharSet "Symbol"
    _ -> case findObjFromDict (fontDescriptorFromDict d objs) "/FontFile3" of
           Just (ObjRef fontfile) ->
             case rawStreamByRef sec objs fontfile of
               Right bs -> CFF.encoding $ BSL.toStrict bs
               Left _ -> NullMap
           _ -> case findObjFromDict (fontDescriptorFromDict d objs) "/FontFile" of
             Just (ObjRef fontfile) ->
               case rawStreamByRef sec objs fontfile of
                 Right bs -> Type1.encoding $ BSL.toStrict bs
                 Left _ -> NullMap
             _ -> NullMap
  Just (PdfName "/Type2") -> NullMap
  Just (PdfName "/Type3") -> NullMap
  _ -> NullMap

  where
    subtype = M.lookup "/Subtype" d
    encField = M.lookup "/Encoding" d

    sjisEncodings :: [T.Text]
    sjisEncodings =
      [ "/90ms-RKSJ-H", "/90ms-RKSJ-V"
      , "/90msp-RKSJ-H", "/90msp-RKSJ-V"
      , "/RKSJ-H", "/RKSJ-V"
      ]

    unijisEncodings :: [T.Text]
    unijisEncodings =
      [ "/UniJIS-UCS2-H", "/UniJIS-UCS2-V"
      , "/UniJIS-UCS2-HW-H", "/UniJIS-UCS2-HW-V"
      , "/UniJIS-UTF16-H", "/UniJIS-UTF16-V"
      , "/UniJIS2004-UTF16-H", "/UniJIS2004-UTF16-V"
      ]

    descendantFonts :: [Obj]
    descendantFonts = descendantFontObjsFromDict d objs

    cidSysInfo :: [Obj] -> [Encoding]
    cidSysInfo [] = []
    cidSysInfo ((ObjRef r):rs) = cidSysInfo' r : cidSysInfo rs
    cidSysInfo' dfr = case findObjFromDictWithRef dfr "/CIDSystemInfo" objs of
      Just (PdfDict dict) -> getCIDSystemInfo dict
      Just (ObjRef r) -> case findDictByRef r objs of
                           Just dict -> getCIDSystemInfo dict
                           _ -> WithCharSet T.empty
      _ -> WithCharSet T.empty

    getCIDSystemInfo cidDict =
      let registry = case findObjFromDict cidDict "/Registry" of
                       Just (PdfText r) -> r
                       _ -> T.empty
          ordering = case findObjFromDict cidDict "/Ordering" of
                       Just (PdfText o) -> o
                       _ -> T.empty
          cmap = registry `T.append` "-" `T.append` ordering
      in CIDmap cmap


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


findCMap :: Dict -> Maybe Security -> PDFObjIndex -> Map T.Text CMap
findCMap d sec objs = M.fromListWith (flip const) $
  [ (n, toUnicode sec r objs) | (n, ObjRef r) <- M.toList (fontObjs d objs) ]

toUnicode :: Maybe Security -> Int -> PDFObjIndex -> CMap
toUnicode sec x objs =
  case findDictByRef x objs of
    Just d -> toUnicodeFromDict sec objs d
    Nothing -> M.empty

toUnicodeFromDict :: Maybe Security -> PDFObjIndex -> Dict -> CMap
toUnicodeFromDict sec objs d =
  case findObjFromDict d "/ToUnicode" of
    Just (ObjRef ref) ->
      case rawStreamByRef sec objs ref of
        Right s | BSL.null s -> noToUnicodeFromDict sec objs d
                | otherwise  -> parseCMap s
        _ -> noToUnicodeFromDict sec objs d
    _ -> noToUnicodeFromDict sec objs d

noToUnicode :: Maybe Security -> Int -> PDFObjIndex -> CMap
noToUnicode sec x objs =
  case findDictByRef x objs of
    Just d -> noToUnicodeFromDict sec objs d
    Nothing -> M.empty

noToUnicodeFromDict :: Maybe Security -> PDFObjIndex -> Dict -> CMap
noToUnicodeFromDict sec objs d =
  case firstDescendantFontDict d objs of
    Nothing -> M.empty
    Just cidDict ->
      let fd = fontDescriptorFromDict cidDict objs
      in case findObjFromDict fd "/FontFile2" of
           Just (ObjRef fontfile) ->
             case rawStreamByRef sec objs fontfile of
               Right bs -> OpenType.cmap $ BSL.toStrict bs
               Left _ -> M.empty
           _ -> M.empty


fontInfo :: Maybe Security -> Int -> PDFObjIndex -> FontInfo
fontInfo sec x objs =
  fontInfoFromDict sec objs (fromMaybe M.empty $ findDictByRef x objs)

fontInfoFromDict :: Maybe Security -> PDFObjIndex -> Dict -> FontInfo
fontInfoFromDict sec objs d =
  case M.lookup "/Subtype" d of
    Just (PdfName "/Type0") -> type0FontInfoDict sec objs d
    _ -> simpleFontInfoDict sec objs d

simpleFontInfoDict :: Maybe Security -> PDFObjIndex -> Dict -> FontInfo
simpleFontInfoDict sec objs d =
  let enc = encodingFromDict sec objs d
      tuc = toUnicodeFromDict sec objs d
      fd = fontDescriptorFromDict d objs
      defaultW = case findObjFromDict fd "/MissingWidth" of
        Just (PdfNumber w) -> w
        _ -> 0
      firstChar = case findObjFromDict d "/FirstChar" of
        Just (PdfNumber n) -> truncate n
        _ -> 0
      widths = case findObjFromDict d "/Widths" of
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

type0FontInfoDict :: Maybe Security -> PDFObjIndex -> Dict -> FontInfo
type0FontInfoDict sec objs d =
  let enc = encodingFromDict sec objs d
      tuc = toUnicodeFromDict sec objs d
      cidDict = firstDescendantFontDict d objs
      defaultW = case cidDict >>= \cd -> findObjFromDict cd "/DW" of
        Just (PdfNumber w) -> w
        _ -> 1000
      widthMap = case cidDict >>= \cd -> findObjFromDict cd "/W" of
        Just wobj -> parseCIDWidths (resolveObjArray wobj objs)
        _ -> M.empty
      (_, w1Default) = dw2Defaults cidDict
      widthVMap = case cidDict >>= \cd -> findObjFromDict cd "/W2" of
        Just wobj -> parseCIDVerticalWidths (resolveObjArray wobj objs)
        _ -> M.empty
      wmode = wmodeFromEncoding (findObjFromDict d "/Encoding")
      widthFn cid = M.findWithDefault defaultW cid widthMap
      widthVFn cid = M.findWithDefault w1Default cid widthVMap
      bpc = case enc of
        SJISmap -> 1
        _ -> 2
  in FontInfo
    { fiEncoding = enc
    , fiToUnicode = tuc
    , fiWidth = widthFn
    , fiWidthV = widthVFn
    , fiWMode = wmode
    , fiBytesPerCode = bpc
    , fiDefaultWidth = defaultW
    }

defaultVerticalW1 :: Double
defaultVerticalW1 = -1000

fontDescriptorFor :: Int -> PDFObjIndex -> Dict
fontDescriptorFor fdr objs = case findDictByRef fdr objs of
  Just d -> fontDescriptorFromDict d objs
  Nothing -> M.empty

fontDescriptorFromDict :: Dict -> PDFObjIndex -> Dict
fontDescriptorFromDict d _objs = case findObjFromDict d "/FontDescriptor" of
  Just (ObjRef r) -> fromMaybe M.empty $ findDictByRef r _objs
  Just (PdfDict fd) -> fd
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
  case findDictByRef fontRef objs of
    Just d -> firstDescendantFontDict d objs
    Nothing -> Nothing

firstDescendantFontDict :: Dict -> PDFObjIndex -> Maybe Dict
firstDescendantFontDict d objs =
  case descendantFontObjsFromDict d objs of
    (df:_) -> cidFontDict df objs
    _ -> Nothing

descendantFontObjs :: Int -> PDFObjIndex -> [Obj]
descendantFontObjs fontRef objs =
  case findDictByRef fontRef objs of
    Just d -> descendantFontObjsFromDict d objs
    Nothing -> []

descendantFontObjsFromDict :: Dict -> PDFObjIndex -> [Obj]
descendantFontObjsFromDict d objs =
  case findObjFromDict d "/DescendantFonts" of
    Just (PdfArray dfrs) -> dfrs
    Just (ObjRef r) -> case findObjsByRef r objs of
      Just (PdfArray dfrs : _) -> dfrs
      Just os | isDescendantFontObj os -> [ObjRef r]
      Just os -> fromMaybe [] (findFirstArray os)
      Nothing -> []
    _ -> []
  where
    isDescendantFontObj os = case findDict os of
      Just cd -> case M.lookup "/Subtype" cd of
        Just (PdfName "/CIDFontType0") -> True
        Just (PdfName "/CIDFontType2") -> True
        _ -> False
      Nothing -> False

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
wmodeFromEncoding (Just (PdfName "/V")) = 1
wmodeFromEncoding (Just (PdfName n)) | "-V" `T.isSuffixOf` n = 1
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

