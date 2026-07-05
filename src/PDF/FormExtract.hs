{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.FormExtract
Description : Form XObject extraction to standalone PDF files (0.4.5)
License     : MIT

Extracts a named Form XObject from a page (including nested resources)
and writes a minimal single-page PDF that draws the form as vector content.

@example
import PDF.FormExtract (pageFormNames, extractFormPdf)

names <- pageFormNames doc 1
pdfBytes <- extractFormPdf doc 1 "Fm42"
-}
module PDF.FormExtract
  ( pageFormNames
  , extractFormPdf
  , extractFormToFile
  ) where

import PDF.Definition
import PDF.Document (Document(..))
import PDF.DocumentStructure
  ( findDict
  , findDictByRef
  , findObjsByRef
  , findResourcesDict
  )
import PDF.Error (PdfError(..), PdfResult)
import PDF.Page (pageRefAt)

import Data.Char (chr, isAscii, isHexDigit, ord)
import Data.List (find, sort, sortOn, nub)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)

import Data.Text.Encoding (encodeUtf16BE)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

pdfHeader :: BS.ByteString
pdfHeader = BSC.pack "%PDF-1.5\n%\xc2\xb5\xc2\xb6\n"

-- | Top-level Form XObject names on a page (page /Resources /XObject only, inherited).
-- Nested forms inside a Form are not listed.
pageFormNames :: Document -> Int -> PdfResult [T.Text]
pageFormNames doc pageNum = do
  pref <- pageRefAt doc pageNum
  xobj <- pageXObjectDict doc pref
  let objs = docObjs doc
  return $ sort $ map stripNameSlash $ filter (isFormName objs xobj) (M.keys xobj)

extractFormPdf :: Document -> Int -> T.Text -> PdfResult BS.ByteString
extractFormPdf doc pageNum name = do
  pref <- pageRefAt doc pageNum
  formRef <- findFormRef doc pref name
  formDict <- formDictAt doc formRef
  bbox <- formBBox formDict
  matrix <- formMatrixArray formDict
  let formKey = formNameKey name
      index = docObjs doc
  copiedRefs <- transitiveClosure formRef index
  let renum = renumberMap copiedRefs
      formNewRef = renum M.! formRef
      pageContent = pageDrawStream formKey matrix
      wrapper =
        [ (1, catalogBody)
        , (2, pagesBody)
        , (3, pageBody bbox formKey formNewRef)
        , (4, contentStreamBody pageContent)
        ]
      copied =
        [ (renum M.! oldRef, serializeCopiedObject oldRef index renum)
        | oldRef <- copiedRefs
        ]
      allObjs = wrapper ++ copied
      (body, offsets) = buildBody allObjs
      xrefPos = BS.length body
      maxObj = maximum (map fst allObjs)
      size = maxObj + 1
  return $
    body
      <> xrefTable offsets size
      <> trailerPart size xrefPos

extractFormToFile :: Document -> Int -> T.Text -> FilePath -> IO (PdfResult FilePath)
extractFormToFile doc pageNum name outDir =
  createDirectoryIfMissing True outDir >> go
  where
    go = case extractFormPdf doc pageNum name of
      Left err -> return (Left err)
      Right bytes -> do
        let path = outDir </> (T.unpack (stripNameSlash name) ++ ".pdf")
        BS.writeFile path bytes
        return (Right path)

-- | Page resources with inheritance (same walk as 'PDF.Interpret.pageResourcesInherited').
pageResourcesInherited :: Dict -> PDFObjIndex -> Maybe Dict
pageResourcesInherited dict objs =
  case findResourcesDict dict objs of
    Just r -> Just r
    Nothing ->
      case M.lookup "/Parent" dict of
        Just (ObjRef pref) ->
          case findDictByRef pref objs of
            Just parent -> pageResourcesInherited parent objs
            Nothing -> Nothing
        _ -> Nothing

pageXObjectDict :: Document -> Int -> PdfResult (M.Map T.Text Obj)
pageXObjectDict doc pageRef = do
  pageDict <- case findObjsByRef pageRef (docObjs doc) of
    Just os -> case findDictOfTypePage os of
      Just d -> Right d
      Nothing -> Left (MissingKey "/Type" ("page " ++ show pageRef))
    Nothing -> Left (MissingObject pageRef)
  case pageResourcesInherited pageDict (docObjs doc) of
    Nothing -> Right M.empty
    Just res -> xobjectEntries res (docObjs doc)

findDictOfTypePage :: [Obj] -> Maybe Dict
findDictOfTypePage os = findDict os

xobjectEntries :: Dict -> PDFObjIndex -> PdfResult (M.Map T.Text Obj)
xobjectEntries res objs = case M.lookup "/XObject" res of
  Nothing -> Right M.empty
  Just (PdfDict xd) -> Right xd
  Just (ObjRef r) ->
    case findDictByRef r objs of
      Just xd -> Right xd
      Nothing -> Left (MissingObject r)
  Just _ -> Left (UnsupportedFeature "invalid /XObject entry on page")

isFormName :: PDFObjIndex -> M.Map T.Text Obj -> T.Text -> Bool
isFormName objs xobj name =
  case M.lookup name xobj of
    Just (ObjRef r) -> isFormObject objs r
    _ -> False

findFormRef :: Document -> Int -> T.Text -> PdfResult Int
findFormRef doc pageRef name = do
  xobj <- pageXObjectDict doc pageRef
  let key = formNameKey name
  case M.lookup key xobj of
    Nothing ->
      Left (MissingKey (T.unpack key) ("page " ++ show pageRef ++ " XObject dictionary"))
    Just (ObjRef r) ->
      if isFormObject (docObjs doc) r
        then Right r
        else Left (UnsupportedFeature (T.unpack key ++ " is not a Form XObject"))
    Just _ ->
      Left (UnsupportedFeature (T.unpack key ++ " is not an indirect Form XObject"))

isFormObject :: PDFObjIndex -> Int -> Bool
isFormObject objs ref =
  case findDictByRef ref objs of
    Just d -> M.lookup "/Subtype" d == Just (PdfName "/Form")
    Nothing -> False

formDictAt :: Document -> Int -> PdfResult Dict
formDictAt doc ref =
  case findDictByRef ref (docObjs doc) of
    Just d -> Right d
    Nothing -> Left (MissingObject ref)

formBBox :: Dict -> PdfResult [Double]
formBBox d = case M.lookup "/BBox" d of
  Just (PdfArray nums) -> parseBBox nums
  Nothing -> Left (MissingKey "/BBox" "Form XObject")
  Just _ -> Left (UnsupportedFeature "Form /BBox must be an array of four numbers")

parseBBox :: [Obj] -> PdfResult [Double]
parseBBox nums =
  case map asNumber nums of
    [Just a, Just b, Just c, Just d'] -> Right [a, b, c, d']
    _ -> Left (UnsupportedFeature "Form /BBox must be four numbers")

asNumber :: Obj -> Maybe Double
asNumber (PdfNumber n) = Just n
asNumber _ = Nothing

formMatrixArray :: Dict -> PdfResult (Maybe [Double])
formMatrixArray d = case M.lookup "/Matrix" d of
  Nothing -> Right Nothing
  Just (PdfArray nums) ->
    case map asNumber nums of
      [Just a, Just b, Just c, Just d', Just e, Just f] -> Right (Just [a, b, c, d', e, f])
      _ -> Left (UnsupportedFeature "Form /Matrix must be six numbers")
  Just _ -> Left (UnsupportedFeature "Form /Matrix must be an array")

formNameKey :: T.Text -> T.Text
formNameKey n =
  if "/" `T.isPrefixOf` n then n else "/" `T.append` n

stripNameSlash :: T.Text -> T.Text
stripNameSlash n =
  if "/" `T.isPrefixOf` n then T.drop 1 n else n

transitiveClosure :: Int -> PDFObjIndex -> PdfResult [Int]
transitiveClosure start objs = go [start] S.empty []
  where
    go [] _ acc = Right (reverse acc)
    go (r : rs) seen acc
      | r `S.member` seen = go rs seen acc
      | otherwise =
          case findObjsByRef r objs of
            Nothing -> Left (MissingObject r)
            Just os ->
              let refs = nub (collectObjRefs os)
                  unseen = filter (not . (`S.member` seen)) refs
              in go (rs ++ unseen) (S.insert r seen) (r : acc)

collectObjRefs :: [Obj] -> [Int]
collectObjRefs = concatMap refsInObj

refsInObj :: Obj -> [Int]
refsInObj (PdfDict d) = concatMap refsInObj (M.elems d)
refsInObj (PdfArray a) = concatMap refsInObj a
refsInObj (ObjRef r) = [r]
refsInObj _ = []

renumberMap :: [Int] -> M.Map Int Int
renumberMap copied = M.fromList (zip copied [5 ..])

rewriteObj :: M.Map Int Int -> Obj -> Obj
rewriteObj mp = go
  where
    go (PdfDict d) = PdfDict (M.map go d)
    go (PdfArray a) = PdfArray (map go a)
    go r@(ObjRef old) = maybe r ObjRef (M.lookup old mp)
    go o = o

catalogBody :: BS.ByteString
catalogBody = BSC.pack "<< /Type /Catalog /Pages 2 0 R >>"

pagesBody :: BS.ByteString
pagesBody = BSC.pack "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"

pageBody :: [Double] -> T.Text -> Int -> BS.ByteString
pageBody bbox formKey formNewRef =
  BSC.pack $
    unwords
      [ "<< /Type /Page /Parent 2 0 R"
      , "/MediaBox [" ++ showBBox bbox ++ "]"
      , "/Resources << /XObject <<"
      , BSC.unpack (serializeName formKey)
      , show formNewRef ++ " 0 R >> >>"
      , "/Contents 4 0 R >>"
      ]

showBBox :: [Double] -> String
showBBox = unwords . map showPdfNumber

pageDrawStream :: T.Text -> Maybe [Double] -> BS.ByteString
pageDrawStream formKey mmat =
  let cmOp =
        case mmat of
          Nothing -> "1 0 0 1 0 0 cm"
          Just [a, b, c, d, e, f]
            | isIdentity [a, b, c, d, e, f] -> "1 0 0 1 0 0 cm"
            | otherwise ->
                unwords (map showPdfNumber [a, b, c, d, e, f]) ++ " cm"
          _ -> "1 0 0 1 0 0 cm"
      ops =
        BSC.concat
          [ "q\n"
          , BSC.pack cmOp
          , "\n"
          , serializeName formKey
          , " Do\nQ\n"
          ]
  in ops

isIdentity :: [Double] -> Bool
isIdentity [a, b, c, d, e, f] =
  near a 1 && near b 0 && near c 0 && near d 1 && near e 0 && near f 0
isIdentity _ = False

near :: Double -> Double -> Bool
near x y = abs (x - y) < 1e-9

contentStreamBody :: BS.ByteString -> BS.ByteString
contentStreamBody streamBytes =
  BSC.concat
    [ "<< /Length "
    , BSC.pack (show (BS.length streamBytes))
    , " >>\nstream\n"
    , streamBytes
    , "\nendstream"
    ]

serializeCopiedObject :: Int -> PDFObjIndex -> M.Map Int Int -> BS.ByteString
serializeCopiedObject ref objs renum =
  case findObjsByRef ref objs of
    Nothing -> "null"
    Just os ->
      case findDict os of
        Just d ->
          case findStream os of
            Nothing ->
              BSC.concat ["<<", serializeDict (rewriteObj renum <$> d), " >>"]
            Just stream ->
              serializeStreamObject (rewriteObj renum <$> d) stream
        Nothing ->
          case os of
            [single] -> serializeObj (rewriteObj renum single)
            _ -> "null"

findStream :: [Obj] -> Maybe BSL.ByteString
findStream os = case find isStream os of
  Just (PdfStream s) -> Just s
  _ -> Nothing
  where
    isStream (PdfStream _) = True
    isStream _ = False

serializeStreamObject :: Dict -> BSL.ByteString -> BS.ByteString
serializeStreamObject d stream =
  let raw = BSL.toStrict stream
      -- PdfStream in the index holds file bytes (still filter-encoded).
      -- Re-compressing would double-apply FlateDecode and break content streams.
      finalDict =
        M.insert "/Length" (PdfNumber (fromIntegral (BS.length raw))) (stripLength d)
  in BSC.concat
       [ "<<"
       , serializeDict finalDict
       , " >>\nstream\n"
       , raw
       , "\nendstream"
       ]
stripLength = M.filterWithKey (\k _ -> k /= "/Length") . M.delete "/Length"

serializeDict :: Dict -> BS.ByteString
serializeDict d =
  BSC.concat [serializeDictEntry k v | (k, v) <- M.toAscList d]

serializeDictEntry :: T.Text -> Obj -> BS.ByteString
serializeDictEntry k v =
  BSC.concat [" ", serializeName k, " ", serializeObj v]

serializeObj :: Obj -> BS.ByteString
serializeObj (PdfDict d) = BSC.concat ["<<", serializeDict d, " >>"]
serializeObj (PdfArray a) =
  BSC.concat ["[", BSC.intercalate " " (map serializeObj a), "]"]
serializeObj (PdfName n) = serializeName n
serializeObj (PdfText t) = serializeText t
serializeObj (PdfHex h) = serializeHex h
serializeObj (PdfNumber n) = BSC.pack (showPdfNumber n)
serializeObj (PdfBool True) = "true"
serializeObj (PdfBool False) = "false"
serializeObj (ObjRef r) = BSC.pack (show r ++ " 0 R")
serializeObj PdfNull = "null"
serializeObj (ObjOther o) = BSC.pack (T.unpack o)

serializeName :: T.Text -> BS.ByteString
serializeName n = BSC.pack (T.unpack n)

serializeText :: T.Text -> BS.ByteString
serializeText t
  | T.all isAscii t =
      let esc c =
            case c of
              '\\' -> "\\"
              '(' -> "\\("
              ')' -> "\\)"
              _ -> [c]
       in BSC.concat ["(", BSC.pack (concatMap esc (T.unpack t)), ")"]
  | otherwise =
      BSC.concat
        [ "<"
        , bytesToHex (BS.pack [0xfe, 0xff] <> encodeUtf16BE t)
        , ">"
        ]

-- | Re-encode 'PdfHex' from parsed objects (hex digits or raw byte chars).
serializeHex :: T.Text -> BS.ByteString
serializeHex h
  | T.all (\c -> isHexDigit c) h = BSC.concat ["<", BSC.pack (T.unpack h), ">"]
  | otherwise =
      BSC.concat ["<", bytesToHex (textRawBytes h), ">"]

textRawBytes :: T.Text -> BS.ByteString
textRawBytes = BS.pack . map (fromIntegral . ord) . T.unpack

bytesToHex :: BS.ByteString -> BS.ByteString
bytesToHex =
  BSC.pack . concatMap byteHex . BS.unpack
  where
    byteHex w =
      let hi = w `div` 16
          lo = w `mod` 16
       in [hexChar hi, hexChar lo]

hexChar :: Integral a => a -> Char
hexChar n
  | n < 10 = chr (fromIntegral n + ord '0')
  | otherwise = chr (fromIntegral n - 10 + ord 'A')

showPdfNumber :: Double -> String
showPdfNumber n
  | n == fromIntegral (truncate n :: Integer) = show (truncate n :: Integer)
  | otherwise = show n

obj :: Int -> BS.ByteString -> BS.ByteString
obj n body = BSC.concat [BSC.pack (show n), " 0 obj\n", body, "\nendobj\n"]

buildBody :: [(Int, BS.ByteString)] -> (BS.ByteString, [(Int, Int)])
buildBody objects = go pdfHeader [] (sortOn fst objects)
  where
    go acc offs [] = (acc, reverse offs)
    go acc offs ((n, b) : rest) =
      go (acc <> obj n b) ((n, BS.length acc) : offs) rest

xrefEntry :: Int -> BS.ByteString
xrefEntry off = BSC.pack (printf "%010d 00000 n \n" off)

xrefTable :: [(Int, Int)] -> Int -> BS.ByteString
xrefTable offsets size =
  BSC.concat $
    [ "xref\n"
    , BSC.pack ("0 " ++ show size ++ "\n")
    , "0000000000 65535 f \n"
    ]
      ++ map (xrefEntry . snd) (sortOn fst offsets)

trailerPart :: Int -> Int -> BS.ByteString
trailerPart size xrefPos =
  BSC.concat
    [ "trailer\n<< /Size "
    , BSC.pack (show size)
    , " /Root 1 0 R >>\nstartxref\n"
    , BSC.pack (show xrefPos)
    , "\n%%EOF\n"
    ]
