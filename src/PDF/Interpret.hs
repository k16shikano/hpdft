{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Content-stream interpreter for glyph geometry.
-- Form XObjects inherit the enclosing marked-content stack (MCID space is per stream in the spec).
module PDF.Interpret
  ( Glyph(..)
  , Rect(..)
  , PageItem(..)
  , interpretPage
  , interpretPageItems
  , interpretPageImageHits
  , interpretContent
  , interpretContentItems
  , interpretContentWithFonts
  , interpretContentWithFontsItems
  , bytesToCodes
  , encodingUnicode
  , unicodeBytesToCodes
  ) where

import PDF.Definition
import PDF.Document (Document(..))
import PDF.DocumentStructure
  ( findDictByRef
  , findDictOfType
  , findObjFromDict
  , findObjsByRef
  , findResourcesDict
  , fontInfo
  , fontInfoFromDict
  , rawStream
  , rawStreamByRef
  )
import PDF.Matrix
import PDF.Character (pdfcharmap, adobeJapanOneSixMap, cp932Map, jisx0208Map)
import PDF.Encrypt (Security)
import PDF.Error (PdfError(..), PdfResult)
import PDF.Object (parseRefsArray)

import Data.Char (chr, isDigit, ord)
import Data.List (find, foldl', isPrefixOf)
import Data.Bits (shiftL)
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word8)
import qualified Numeric as Num

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Map as M
import qualified Data.Text as T

data Glyph = Glyph
  { glyphText     :: T.Text
  , glyphX        :: Double
  , glyphY        :: Double
  , glyphWidth    :: Double
  , glyphSize     :: Double
  , glyphFont     :: T.Text
  , glyphWMode    :: Int
  , glyphMCID     :: Maybe Int
  } deriving (Eq, Show)

data MCEntry = MCEntry
  { mcTag  :: T.Text
  , mcMCID :: Maybe Int
  } deriving (Eq, Show)

data Rect = Rect
  { rectX0 :: Double
  , rectY0 :: Double
  , rectX1 :: Double
  , rectY1 :: Double
  } deriving (Eq, Show)

data PageItem = ItemGlyph Glyph | ItemGraphic Rect deriving (Eq, Show)

data GState = GState
  { ctm        :: Matrix
  , gsFontRes  :: Maybe T.Text
  , gsFont     :: Maybe FontInfo
  , gsFontSize :: Double
  , gsCharSp   :: Double
  , gsWordSp   :: Double
  , gsHScale   :: Double
  , gsLeading  :: Double
  , gsRise     :: Double
  , gsRender   :: Int
  }

data TState = TState
  { tmMat  :: Matrix
  , tlmMat :: Matrix
  }

data IState = IState
  { gsCur         :: GState
  , gsStack       :: [GState]
  , tsCur         :: Maybe TState
  , itemsRev       :: [PageItem]
  , imagesRev     :: [(Int, Rect)]
  , collectImages :: Bool
  , pathAcc       :: [(Double, Double)]
  , depth         :: Int
  , isSec         :: Maybe Security
  , isObjs        :: PDFObjIndex
  , isStreamCache :: M.Map Int (PdfResult BSL.ByteString)
  , isFontCache   :: M.Map Int FontInfo
  , isRes         :: Dict
  , fontOverrides :: M.Map T.Text FontInfo
  , operandStack  :: [Obj]
  , mcStack       :: [MCEntry]
  }

initialGState :: GState
initialGState = GState
  { ctm = identity
  , gsFontRes = Nothing
  , gsFont = Nothing
  , gsFontSize = 0
  , gsCharSp = 0
  , gsWordSp = 0
  , gsHScale = 1
  , gsLeading = 0
  , gsRise = 0
  , gsRender = 0
  }

initialIState
  :: Maybe Security
  -> PDFObjIndex
  -> M.Map Int (PdfResult BSL.ByteString)
  -> M.Map Int FontInfo
  -> Dict
  -> IState
initialIState sec objs streamCache fontCache res = IState
  { gsCur = initialGState
  , gsStack = []
  , tsCur = Nothing
  , itemsRev = []
  , imagesRev = []
  , collectImages = False
  , pathAcc = []
  , depth = 0
  , isSec = sec
  , isObjs = objs
  , isStreamCache = streamCache
  , isFontCache = fontCache
  , isRes = res
  , fontOverrides = M.empty
  , operandStack = []
  , mcStack = []
  }

buildCaches
  :: Maybe Security
  -> PDFObjIndex
  -> (M.Map Int (PdfResult BSL.ByteString), M.Map Int FontInfo)
buildCaches sec objs =
  ( M.mapWithKey (\r os -> rawStream sec r os) objs
  , M.mapWithKey
      (\r _ -> fontInfoFromDict sec objs (fromMaybe M.empty (findDictByRef r objs)))
      objs
  )

lookupStreamCache
  :: Maybe Security
  -> PDFObjIndex
  -> M.Map Int (PdfResult BSL.ByteString)
  -> Int
  -> PdfResult BSL.ByteString
lookupStreamCache sec objs cache ref =
  case M.lookup ref cache of
    Just r -> r
    Nothing -> rawStreamByRef sec objs ref

maxFormDepth :: Int
maxFormDepth = 12

interpretContent :: Maybe Security -> PDFObjIndex -> Dict -> BSL.ByteString -> [Glyph]
interpretContent sec objs res bytes =
  interpretContentWithFonts sec objs res M.empty bytes

interpretContentItems :: Maybe Security -> PDFObjIndex -> Dict -> BSL.ByteString -> [PageItem]
interpretContentItems sec objs res bytes =
  interpretContentWithFontsItems sec objs res M.empty bytes

interpretContentWithFonts :: Maybe Security -> PDFObjIndex -> Dict -> M.Map T.Text FontInfo -> BSL.ByteString -> [Glyph]
interpretContentWithFonts sec objs res fonts bytes =
  [g | ItemGlyph g <- interpretContentWithFontsItems sec objs res fonts bytes]

interpretContentWithFontsItems :: Maybe Security -> PDFObjIndex -> Dict -> M.Map T.Text FontInfo -> BSL.ByteString -> [PageItem]
interpretContentWithFontsItems sec objs res fonts bytes =
  let (streamCache, fontCache) = buildCaches sec objs
      st0 = initialIState sec objs streamCache fontCache res
      st1 = st0 {fontOverrides = fonts}
  in reverse (itemsRev (runStream st1 bytes))

interpretPage :: Document -> Int -> PdfResult [Glyph]
interpretPage doc pageRef = do
  items <- interpretPageItems doc pageRef
  return [g | ItemGlyph g <- items]

interpretPageItems :: Document -> Int -> PdfResult [PageItem]
interpretPageItems doc pageRef = do
  (_, content, res) <- pageInterpretInputs doc pageRef
  let st0 = initialIState
        (docSecurity doc)
        (docObjs doc)
        (docStreamCache doc)
        (docFontCache doc)
        res
  return $ reverse (itemsRev (runStream st0 content))

interpretPageImageHits :: Document -> Int -> PdfResult [(Int, Rect)]
interpretPageImageHits doc pageRef = do
  (_, content, res) <- pageInterpretInputs doc pageRef
  let st0 =
        (initialIState
          (docSecurity doc)
          (docObjs doc)
          (docStreamCache doc)
          (docFontCache doc)
          res)
          {collectImages = True}
  return $ reverse (imagesRev (runStream st0 content))

pageInterpretInputs :: Document -> Int -> PdfResult (Dict, BSL.ByteString, Dict)
pageInterpretInputs doc pageRef = do
  pageDict <- case findObjsByRef pageRef (docObjs doc) of
    Just os -> case findDictOfType "/Page" os of
      Just d -> Right d
      Nothing -> Left (MissingKey "/Type" ("page " ++ show pageRef))
    Nothing -> Left (MissingObject pageRef)
  res <- case pageResourcesInherited pageDict (docObjs doc) of
    Just r -> Right r
    Nothing -> Right M.empty
  content <- pageContentsBytes (docSecurity doc) (docObjs doc) (docStreamCache doc) pageDict
  return (pageDict, content, res)

pageResourcesInherited :: Dict -> PDFObjIndex -> Maybe Dict
pageResourcesInherited dict objs =
  case findResourcesDict dict objs of
    Just r -> Just r
    Nothing -> case M.lookup "/Parent" dict of
      Just (ObjRef pref) ->
        case findDictByRef pref objs of
          Just parent -> pageResourcesInherited parent objs
          Nothing -> Nothing
      _ -> Nothing

pageContentsBytes
  :: Maybe Security
  -> PDFObjIndex
  -> M.Map Int (PdfResult BSL.ByteString)
  -> Dict
  -> PdfResult BSL.ByteString
pageContentsBytes sec objs streamCache dict = case M.lookup "/Contents" dict of
  Nothing -> Left (MissingKey "/Contents" "page")
  Just (PdfArray arr) -> concatContentRefs (parseRefsArray arr)
  Just (ObjRef r) ->
    case findObjsByRef r objs of
      Just [PdfArray arr] -> concatContentRefs (parseRefsArray arr)
      Just _ -> singleStream r
      Nothing -> Left (MissingObject r)
  where
    concatContentRefs refs = do
      parts <- mapM singleStream refs
      return $ BSL.intercalate (BSLC.pack "\n") parts
    singleStream ref = lookupStreamCache sec objs streamCache ref

data Token = TokOperand Obj | TokOperator String

runStream :: IState -> BSL.ByteString -> IState
runStream st bs = loop st (skipWs bs)
  where
    loop s input
      | BSL.null input = s
      | otherwise = case readToken input of
          Just (TokOperand o, rest) ->
            loop (s {operandStack = o : operandStack s}) (skipWs rest)
          Just (TokOperator "BI", rest) ->
            loop (emitInlineImageSt s) (skipInlineImage (skipWs rest))
          Just (TokOperator op, rest) ->
            loop (dispatchOp op s) (skipWs rest)
          Nothing ->
            loop s (BSL.tail input)

dispatchOp :: String -> IState -> IState
dispatchOp op st =
  let st' = execOp op st
  in st' {operandStack = []}

execOp :: String -> IState -> IState
execOp "q" st = pushGStateSt st
execOp "Q" st = popGStateSt st
execOp "cm" st =
  case popNums 6 st of
    Just ([f, e, d, c, b, a], st') ->
      modifyGStateSt (\gs -> gs {ctm = multiply (mkMatrix a b c d e f) (ctm gs)}) st'
    _ -> st
execOp "BT" st = st {tsCur = Just (TState identity identity)}
execOp "ET" st = st {tsCur = Nothing}
execOp "Tf" st =
  case operandStack st of
    PdfNumber size : PdfName font : _ ->
      resolveFontSt font size st
    _ -> st
execOp "Tc" st = setGSDoubleSt (\v gs -> gs {gsCharSp = v}) st
execOp "Tw" st = setGSDoubleSt (\v gs -> gs {gsWordSp = v}) st
execOp "Tz" st =
  case popNums 1 st of
    Just ([v], st') -> modifyGStateSt (\gs -> gs {gsHScale = v / 100}) st'
    _ -> st
execOp "TL" st = setGSDoubleSt (\v gs -> gs {gsLeading = v}) st
execOp "Ts" st = setGSDoubleSt (\v gs -> gs {gsRise = v}) st
execOp "Tr" st =
  case popNums 1 st of
    Just ([v], st') -> modifyGStateSt (\gs -> gs {gsRender = truncate v}) st'
    _ -> st
execOp "Td" st =
  case popNums 2 st of
    Just ([ty, tx], st') -> textTdSt tx ty st'
    _ -> st
execOp "TD" st =
  case popNums 2 st of
    Just ([ty, tx], st') ->
      let st'' = modifyGStateSt (\gs -> gs {gsLeading = -ty}) st'
      in textTdSt tx ty st''
    _ -> st
execOp "Tm" st =
  case popNums 6 st of
    Just ([f, e, d, c, b, a], st') -> textSetMatrixSt (mkMatrix a b c d e f) st'
    _ -> st
execOp "T*" st = withTextMatrixSt textLeadingNewlineSt st
execOp "Tj" st =
  case operandStack st of
    o : _ -> maybe st (`showBytesSt` st) (objBytes o)
    _ -> st
execOp "TJ" st =
  case operandStack st of
    o : _ -> maybe st (`showTJSt` st) (tjElems o)
    _ -> st
execOp "'" st =
  case operandStack st of
    o : _ ->
      withTextMatrixSt
        (\s -> maybe (textLeadingNewlineSt s) (`showBytesSt` (textLeadingNewlineSt s)) (objBytes o))
        st
    _ -> withTextMatrixSt textLeadingNewlineSt st
execOp "\"" st =
  case operandStack st of
    o : PdfNumber ac : PdfNumber aw : _ ->
      let st' = modifyGStateSt (\gs -> gs {gsWordSp = aw, gsCharSp = ac}) st
      in withTextMatrixSt
           (\s -> maybe (textLeadingNewlineSt s) (`showBytesSt` (textLeadingNewlineSt s)) (objBytes o))
           st'
    _ -> st
execOp "Do" st =
  case operandStack st of
    PdfName name : _ -> invokeXObjectSt name st
    _ -> st
execOp "m" st =
  case popNums 2 st of
    Just ([y, x], st') -> st' {pathAcc = [devicePoint st' x y]}
    _ -> st
execOp "l" st =
  case popNums 2 st of
    Just ([y, x], st') -> appendPathPoint st' x y
    _ -> st
execOp "c" st =
  case popNums 6 st of
    Just ([y3, x3, y2, x2, y1, x1], st') ->
      prependPathPoints st'
        [ devicePoint st' x1 y1
        , devicePoint st' x2 y2
        , devicePoint st' x3 y3
        ]
    _ -> st
execOp "v" st =
  case popNums 4 st of
    Just ([y3, x3, y2, x2], st') ->
      prependPathPoints st'
        [devicePoint st' x2 y2, devicePoint st' x3 y3]
    _ -> st
execOp "y" st =
  case popNums 4 st of
    Just ([y3, x3, y1, x1], st') ->
      prependPathPoints st'
        [devicePoint st' x1 y1, devicePoint st' x3 y3]
    _ -> st
execOp "re" st =
  case popNums 4 st of
    Just ([h, w, y, x], st') ->
      prependPathPoints st'
        [ devicePoint st' x y
        , devicePoint st' (x + w) y
        , devicePoint st' x (y + h)
        , devicePoint st' (x + w) (y + h)
        ]
    _ -> st
execOp "h" st = st
execOp "n" st = st {pathAcc = []}
execOp "S" st = paintPathSt st
execOp "s" st = paintPathSt st
execOp "f" st = paintPathSt st
execOp "F" st = paintPathSt st
execOp "f*" st = paintPathSt st
execOp "B" st = paintPathSt st
execOp "B*" st = paintPathSt st
execOp "b" st = paintPathSt st
execOp "b*" st = paintPathSt st
execOp "W" st = st
execOp "W*" st = st
execOp "BDC" st =
  case operandStack st of
    props : PdfName tag : _ ->
      let mcid = mcidFromProps props (isRes st) (isObjs st)
      in pushMCEntry tag mcid st
    _ -> st
execOp "BMC" st =
  case operandStack st of
    PdfName tag : _ -> pushMCEntry tag Nothing st
    _ -> st
execOp "EMC" st =
  case mcStack st of
    _ : rest -> st {mcStack = rest}
    []       -> st
execOp _ st = st

pushMCEntry :: T.Text -> Maybe Int -> IState -> IState
pushMCEntry tag mcid st =
  st {mcStack = MCEntry tag mcid : mcStack st}

currentMCID :: IState -> Maybe Int
currentMCID st = enclosingMCID (mcStack st)

enclosingMCID :: [MCEntry] -> Maybe Int
enclosingMCID [] = Nothing
enclosingMCID (e : es) =
  case mcMCID e of
    Just n  -> Just n
    Nothing -> enclosingMCID es

mcidFromProps :: Obj -> Dict -> PDFObjIndex -> Maybe Int
mcidFromProps props res objs =
  case resolvePropsDict props res objs of
    Just d -> mcidFromDict d
    Nothing -> Nothing

resolvePropsDict :: Obj -> Dict -> PDFObjIndex -> Maybe Dict
resolvePropsDict (PdfDict d) _ _ = Just d
resolvePropsDict (PdfName n) res objs =
  case findObjFromDict res "/Properties" of
    Just (PdfDict pd) -> case M.lookup n pd of
      Just (PdfDict d) -> Just d
      Just (ObjRef r) ->
        case findDictByRef r objs of
          Just d -> Just d
          Nothing -> Nothing
      _ -> Nothing
    Just (ObjRef r) ->
      case findDictByRef r objs of
        Just pd -> case M.lookup n pd of
          Just (PdfDict d) -> Just d
          Just (ObjRef r') ->
            case findDictByRef r' objs of
              Just d -> Just d
              Nothing -> Nothing
          _ -> Nothing
        Nothing -> Nothing
    _ -> Nothing
resolvePropsDict _ _ _ = Nothing

mcidFromDict :: Dict -> Maybe Int
mcidFromDict d = case M.lookup "/MCID" d of
  Just (PdfNumber n) -> Just (truncate n)
  _                  -> Nothing

devicePoint :: IState -> Double -> Double -> (Double, Double)
devicePoint st x y = apply (ctm (gsCur st)) (x, y)

appendPathPoint :: IState -> Double -> Double -> IState
appendPathPoint st x y = st {pathAcc = devicePoint st x y : pathAcc st}

prependPathPoints :: IState -> [(Double, Double)] -> IState
prependPathPoints st pts = st {pathAcc = foldl (flip (:)) (pathAcc st) pts}

pointsBbox :: [(Double, Double)] -> Rect
pointsBbox pts =
  let xs = map fst pts
      ys = map snd pts
  in Rect (minimum xs) (minimum ys) (maximum xs) (maximum ys)

paintPathSt :: IState -> IState
paintPathSt st =
  case pathAcc st of
    [] -> st
    pts -> st {itemsRev = ItemGraphic (pointsBbox (reverse pts)) : itemsRev st, pathAcc = []}

emitGraphicSt :: Rect -> IState -> IState
emitGraphicSt r st = st {itemsRev = ItemGraphic r : itemsRev st}

ctmUnitSquare :: Matrix -> Rect
ctmUnitSquare m = pointsBbox [apply m (0,0), apply m (1,0), apply m (0,1), apply m (1,1)]

emitInlineImageSt :: IState -> IState
emitInlineImageSt st =
  emitGraphicSt (ctmUnitSquare (ctm (gsCur st))) st {operandStack = []}

setGSDoubleSt :: (Double -> GState -> GState) -> IState -> IState
setGSDoubleSt f st =
  case popNums 1 st of
    Just ([v], st') -> modifyGStateSt (f v) st'
    _ -> st

popNums :: Int -> IState -> Maybe ([Double], IState)
popNums n st = go n (operandStack st) []
  where
    go 0 stack acc = Just (reverse acc, st {operandStack = stack})
    go k (PdfNumber x : rest) acc = go (k - 1) rest (x : acc)
    go _ _ _ = Nothing

pushGStateSt :: IState -> IState
pushGStateSt st = st {gsStack = gsCur st : gsStack st}

popGStateSt :: IState -> IState
popGStateSt st =
  case gsStack st of
    (g : gs) -> st {gsCur = g, gsStack = gs}
    []       -> st

modifyGStateSt :: (GState -> GState) -> IState -> IState
modifyGStateSt f st = st {gsCur = f (gsCur st)}

resolveFontSt :: T.Text -> Double -> IState -> IState
resolveFontSt fontName size st =
  let fi = lookupFont (isRes st) fontName st
  in modifyGStateSt
       (\gs -> gs {gsFontRes = Just fontName, gsFont = fi, gsFontSize = size})
       st

lookupFont res fontName st =
  case M.lookup fontName (fontOverrides st) of
    Just fi -> Just fi
    Nothing -> lookupFontResource (isFontCache st) (isSec st) (isObjs st) res fontName

lookupFontResource
  :: M.Map Int FontInfo
  -> Maybe Security
  -> PDFObjIndex
  -> Dict
  -> T.Text
  -> Maybe FontInfo
lookupFontResource fontCache sec objs res fontName =
  case findObjFromDict res "/Font" of
    Just (PdfDict fd) -> fontFromDict fontCache sec objs fd fontName
    Just (ObjRef r) ->
      case findDictByRef r objs of
        Just fd -> fontFromDict fontCache sec objs fd fontName
        Nothing -> Nothing
    _ -> Nothing

fontFromDict
  :: M.Map Int FontInfo
  -> Maybe Security
  -> PDFObjIndex
  -> Dict
  -> T.Text
  -> Maybe FontInfo
fontFromDict fontCache sec objs fd name =
  case M.lookup name fd of
    Just (ObjRef r) ->
      Just (repairCidFontInfo name (M.findWithDefault (fontInfo sec r objs) r fontCache))
    Just (PdfDict d) -> Just (repairCidFontInfo name (fontInfoFromDict sec objs d))
    _ -> Nothing

repairCidFontInfo :: T.Text -> FontInfo -> FontInfo
repairCidFontInfo name fi
  | not (isCidFontName name) = fi
  | fiBytesPerCode fi == 2, CIDmap{} <- fiEncoding fi = fi
  | fiBytesPerCode fi == 2, Encoding{} <- fiEncoding fi = fi
  | SJISmap <- fiEncoding fi = fi
  | UnicodeMap <- fiEncoding fi = fi
  | JISmap <- fiEncoding fi = fi
  | otherwise = defaultAdobeJapanFontInfo fi

isCidFontName :: T.Text -> Bool
isCidFontName name =
  any (`T.isPrefixOf` name) ["/C0_", "/C1_", "/C2_", "/C3_"]

defaultAdobeJapanFontInfo :: FontInfo -> FontInfo
defaultAdobeJapanFontInfo fi =
  let dw = if fiDefaultWidth fi == 0 then 1000 else fiDefaultWidth fi
      widthFn cid = let w = fiWidth fi cid in if w == 0 then dw else w
  in fi
    { fiEncoding = CIDmap "Adobe-Japan1"
    , fiBytesPerCode = 2
    , fiDefaultWidth = dw
    , fiWidth = widthFn
    , fiWidthV = \cid -> let w = fiWidthV fi cid in if w == 0 then defaultVerticalW1 else w
    }

textTdSt :: Double -> Double -> IState -> IState
textTdSt tx ty st =
  case tsCur st of
    Nothing -> st
    Just ts ->
      let tlm' = multiply (translate tx ty) (tlmMat ts)
      in st {tsCur = Just (TState {tmMat = tlm', tlmMat = tlm'})}

textSetMatrixSt :: Matrix -> IState -> IState
textSetMatrixSt m st = st {tsCur = Just (TState {tmMat = m, tlmMat = m})}

textLeadingNewlineSt :: IState -> IState
textLeadingNewlineSt st =
  let leading = -(gsLeading (gsCur st))
  in textTdSt 0 leading st

withTextMatrixSt :: (IState -> IState) -> IState -> IState
withTextMatrixSt f st = if isJust (tsCur st) then f st else st

data TJElem = TJString [Int] | TJAdjust Double

showTJSt :: [TJElem] -> IState -> IState
showTJSt elems st = foldl' go st elems
  where
    go s (TJString bs) = showBytesSt bs s
    go s (TJAdjust k)  = tjKernSt k s

tjKernSt :: Double -> IState -> IState
tjKernSt k st =
  case tsCur st of
    Nothing -> st
    Just ts ->
      let gs = gsCur st
          fi = gsFont gs
          wmode = maybe 0 fiWMode fi
          disp = -k / 1000 * gsFontSize gs * gsHScale gs
          tm' = if wmode == 1
                then multiply (translate 0 disp) (tmMat ts)
                else multiply (translate disp 0) (tmMat ts)
      in st {tsCur = Just ts {tmMat = tm'}}

showBytesSt :: [Int] -> IState -> IState
showBytesSt bytes st =
  case (tsCur st, gsFont (gsCur st), gsFontRes (gsCur st)) of
    (Nothing, _, _) -> st
    (_, Nothing, _) -> st
    (_, _, Nothing) -> st
    (Just ts, Just fi, Just fname) ->
      let gs = gsCur st
          codes = bytesToCodes fi bytes
          originTrm = textRenderingMatrix gs ts
          (ox, oy) = apply originTrm (0, 0)
          segSize = deviceFontSize originTrm
          (text, endTm) = foldl' (glyphStep gs fi) (T.empty, tmMat ts) codes
          endTrm = textRenderingMatrix gs (ts {tmMat = endTm})
          (ex, ey) = apply endTrm (0, 0)
          width = dist ox oy ex ey
          glyph = Glyph
            { glyphText = text
            , glyphX = ox
            , glyphY = oy
            , glyphWidth = width
            , glyphSize = segSize
            , glyphFont = fname
            , glyphWMode = fiWMode fi
            , glyphMCID = currentMCID st
            }
      in st {itemsRev = ItemGlyph glyph : itemsRev st, tsCur = Just ts {tmMat = endTm}}
    _ -> st

glyphStep :: GState -> FontInfo -> (T.Text, Matrix) -> Int -> (T.Text, Matrix)
glyphStep gs fi (txt, tm) code =
  let u = codeToUnicode fi code
      (tx, ty) = codeAdvance gs fi code
      tm' = multiply (translate tx ty) tm
  in (txt `T.append` u, tm')

bytesToCodes :: FontInfo -> [Int] -> [Int]
bytesToCodes fi bytes =
  case fiEncoding fi of
    SJISmap -> sjisBytesToCodes bytes
    UnicodeMap -> unicodeBytesToCodes bytes
    JISmap -> jisBytesToCodes bytes
    _ | fiBytesPerCode fi == 2 -> pairs bytes
    _ -> bytes
  where
    pairs [] = []
    pairs [_] = []
    pairs (a:b:rest) = (a * 256 + b) : pairs rest

isUtf16HighSurrogate :: Int -> Bool
isUtf16HighSurrogate u = u >= 0xD800 && u <= 0xDBFF

isUtf16LowSurrogate :: Int -> Bool
isUtf16LowSurrogate u = u >= 0xDC00 && u <= 0xDFFF

surrogatePairToCode :: Int -> Int -> Int
surrogatePairToCode hi lo = 0x10000 + ((hi - 0xD800) `shiftL` 10) + (lo - 0xDC00)

unicodeBytesToCodes :: [Int] -> [Int]
unicodeBytesToCodes [] = []
unicodeBytesToCodes [_] = []
unicodeBytesToCodes (a:b:rest) =
  let unit = a * 256 + b
  in if isUtf16HighSurrogate unit
     then case rest of
       (c:d:rs) ->
         let unit2 = c * 256 + d
         in if isUtf16LowSurrogate unit2
            then surrogatePairToCode unit unit2 : unicodeBytesToCodes rs
            else unit : unicodeBytesToCodes rest
       _ -> [unit]
     else unit : unicodeBytesToCodes rest

jisBytesToCodes :: [Int] -> [Int]
jisBytesToCodes [] = []
jisBytesToCodes [_] = []
jisBytesToCodes (a:b:rest) = (a * 256 + b) : jisBytesToCodes rest

isSjisLead :: Int -> Bool
isSjisLead b = (b >= 0x81 && b <= 0x9F) || (b >= 0xE0 && b <= 0xFC)

sjisBytesToCodes :: [Int] -> [Int]
sjisBytesToCodes [] = []
sjisBytesToCodes (b:rest)
  | isSjisLead b = case rest of
      (t:rs) -> (b * 256 + t) : sjisBytesToCodes rs
      _ -> [b]
  | otherwise = b : sjisBytesToCodes rest

codeToUnicode :: FontInfo -> Int -> T.Text
codeToUnicode fi code =
  case M.lookup code (fiToUnicode fi) of
    Just s -> s
    Nothing ->
      case fiEncoding fi of
        NullMap | fiBytesPerCode fi == 2 ->
          encodingUnicode (CIDmap "Adobe-Japan1") code
        enc -> encodingUnicode enc code

encodingUnicode :: Encoding -> Int -> T.Text
encodingUnicode (Encoding enc) code =
  case M.lookup (chr code) enc of
    Just glyph ->
      case bulletUnicode glyph of
        Just u -> u
        Nothing ->
          case M.lookup (T.unpack glyph) pdfcharmap of
            Just u -> u
            Nothing -> if "/uni" `T.isPrefixOf` glyph
                       then readUniGlyph glyph
                       else glyph
    Nothing -> T.singleton (safeChr code)
encodingUnicode (CIDmap "Adobe-Japan1") code =
  case M.lookup code adobeJapanOneSixMap of
    Just bs -> T.pack (BSLU.toString bs)
    Nothing -> T.singleton (safeChr code)
encodingUnicode (CIDmap _) code = T.singleton (safeChr code)
encodingUnicode SJISmap code =
  case M.lookup code cp932Map of
    Just bs -> T.pack (BSLU.toString bs)
    Nothing ->
      if code >= 0 && code <= 0x7F
      then T.singleton (safeChr code)
      else "\xFFFD"
encodingUnicode UnicodeMap code = T.singleton (safeChr code)
encodingUnicode JISmap code =
  case M.lookup code jisx0208Map of
    Just bs -> T.pack (BSLU.toString bs)
    Nothing ->
      if code >= 0 && code <= 0x7F
      then T.singleton (safeChr code)
      else "\xFFFD"
encodingUnicode (WithCharSet "ZapfDingbats") code =
  fromMaybe (T.singleton (safeChr code)) (dingbatCodeUnicode code)
encodingUnicode (WithCharSet _) code = T.singleton (safeChr code)
encodingUnicode NullMap code = T.singleton (safeChr code)

bulletUnicode :: T.Text -> Maybe T.Text
bulletUnicode glyph
  | glyph `elem` ["/bullet", "/circle", "/disc", "/filledbox"] = Just "\x2022"
  | otherwise = Nothing

dingbatCodeUnicode :: Int -> Maybe T.Text
dingbatCodeUnicode code
  | code `elem` [108, 110, 114, 183] = Just "\x2022"
  | otherwise = Nothing

readUniGlyph :: T.Text -> T.Text
readUniGlyph s =
  case Num.readHex (T.unpack $ T.drop 4 s) of
    [(i, "")] -> T.singleton (chr i)
    _         -> s

safeChr :: Int -> Char
safeChr n
  | n >= 0 && n <= 0x10FFFF = chr n
  | otherwise = '\xfffd'

fontWidthUnits :: FontInfo -> Int -> Double
fontWidthUnits fi code =
  let w = fiWidth fi code
  in if w == 0 then fiDefaultWidth fi else w

fontWidthVUnits :: FontInfo -> Int -> Double
fontWidthVUnits fi code =
  let w = fiWidthV fi code
  in if w == 0 then defaultVerticalW1 else w

defaultVerticalW1 :: Double
defaultVerticalW1 = -1000

codeAdvance :: GState -> FontInfo -> Int -> (Double, Double)
codeAdvance gs fi code =
  let tfs = gsFontSize gs
      tc = gsCharSp gs
      tw = gsWordSp gs
      th = gsHScale gs
      w0 = fontWidthUnits fi code / 1000
  in if fiWMode fi == 1
     then (0, (fontWidthVUnits fi code / 1000) * tfs + tc + tw)
     else let space = if fiBytesPerCode fi == 1 && code == 32 then tw else 0
          in ((w0 * tfs + tc + space) * th, 0)

textRenderingMatrix :: GState -> TState -> Matrix
textRenderingMatrix gs ts =
  let tfs = gsFontSize gs
      th = gsHScale gs
      tr = gsRise gs
      textMat = mkMatrix (tfs * th) 0 0 tfs 0 tr
  in multiply (multiply textMat (tmMat ts)) (ctm gs)

deviceFontSize :: Matrix -> Double
deviceFontSize trm =
  let (vx, vy) = applyVec trm (0, 1)
  in sqrt (vx * vx + vy * vy)

dist :: Double -> Double -> Double -> Double -> Double
dist x1 y1 x2 y2 =
  sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

invokeXObjectSt :: T.Text -> IState -> IState
invokeXObjectSt name st =
  case findObjFromDict (isRes st) "/XObject" of
    Just (PdfDict xd) -> case M.lookup name xd of
      Just (ObjRef r) -> runXObjectSt r st
      _ -> st
    Just (ObjRef xr) ->
      case findDictByRef xr (isObjs st) of
        Just xd -> case M.lookup name xd of
          Just (ObjRef r) -> runXObjectSt r st
          _ -> st
        Nothing -> st
    _ -> st

runXObjectSt :: Int -> IState -> IState
runXObjectSt ref st0
  | depth st0 >= maxFormDepth = st0
  | otherwise =
      case findObjsByRef ref (isObjs st0) of
        Just os -> case findDict os of
          Just d -> case M.lookup "/Subtype" d of
            Just (PdfName "/Form") ->
              case lookupStreamCache (isSec st0) (isObjs st0) (isStreamCache st0) ref of
                Right stream ->
                  let formMat = formMatrix d
                      formRes = fromMaybe (isRes st0) (findResourcesDict d (isObjs st0))
                      stPush = pushGStateSt st0
                      stMat = modifyGStateSt (\gs -> gs {ctm = multiply formMat (ctm gs)}) stPush
                      stRun = stMat {isRes = formRes, depth = depth st0 + 1, operandStack = []}
                      stDone = runStream stRun stream
                      stPop = popGStateSt st0
                  in stPop
                       { itemsRev = itemsRev stDone
                       , imagesRev = imagesRev stDone
                       , depth = depth st0
                       , isRes = isRes st0
                       }
                Left _ -> st0
            Just (PdfName "/Image") ->
              let bbox = ctmUnitSquare (ctm (gsCur st0))
                  st1 =
                    if collectImages st0
                      then st0 {imagesRev = (ref, bbox) : imagesRev st0}
                      else st0
               in emitGraphicSt bbox st1
            _ -> st0
          Nothing -> st0
        Nothing -> st0

formMatrix :: Dict -> Matrix
formMatrix d = case M.lookup "/Matrix" d of
  Just (PdfArray [PdfNumber a, PdfNumber b, PdfNumber c, PdfNumber d', PdfNumber e, PdfNumber f]) ->
    mkMatrix a b c d' e f
  _ -> identity

objBytes :: Obj -> Maybe [Int]
objBytes (PdfText s) = Just (map ord (T.unpack s))
objBytes (PdfHex h) = Just (hexPairs (T.unpack h))
objBytes _ = Nothing

tjElems :: Obj -> Maybe [TJElem]
tjElems (PdfArray objs) = mapM elemObj objs
  where
    elemObj (PdfNumber n) = Just (TJAdjust n)
    elemObj o = TJString <$> objBytes o
tjElems _ = Nothing

hexPairs :: String -> [Int]
hexPairs [] = []
hexPairs [x] =
  case Num.readHex [x, '0'] of
    [(n, "")] -> [n]
    _         -> []
hexPairs (a:b:rest) =
  case Num.readHex [a, b] of
    [(n, "")] -> n : hexPairs rest
    _         -> hexPairs rest

w2c :: Word8 -> Char
w2c = chr . fromIntegral

isWs8 :: Word8 -> Bool
isWs8 w = w2c w `elem` (" \t\r\n\f" :: String)

delimChar8 :: Word8 -> Bool
delimChar8 w = w2c w `elem` ("[]()<>/{" :: String)

nameEnd8 :: Word8 -> Bool
nameEnd8 w = isWs8 w || delimChar8 w

hexDigit8 :: Word8 -> Bool
hexDigit8 w = w2c w `elem` ("0123456789abcdefABCDEF" :: String)

keywordEnd8 :: BSL.ByteString -> Bool
keywordEnd8 bs = case BSL.uncons bs of
  Nothing -> True
  Just (w, _) -> isWs8 w || w == 37 || delimChar8 w

skipWs :: BSL.ByteString -> BSL.ByteString
skipWs bs = case BSL.uncons bs of
  Nothing -> bs
  Just (w, rest)
    | isWs8 w -> skipWs rest
    | w == 37 -> skipWs (BSL.dropWhile (\w' -> w2c w' `notElem` ("\r\n" :: String)) rest)
    | otherwise -> bs

readToken :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readToken bs =
  let bs' = skipWs bs
  in if BSL.null bs'
     then Nothing
     else case BSL.uncons bs' of
       Nothing -> Nothing
       Just (w, _) ->
         case w of
           91  -> readArray bs'
           60  -> if BSL.take 2 bs' == BSLC.pack "<<" then readDict bs' else readHexString bs'
           40  -> readLiteral bs'
           47  -> readName bs'
           45  -> readNumber bs'
           43  -> readNumber bs'
           46  -> readNumber bs'
           39  -> Just (TokOperator "'", BSL.tail bs')
           34  -> Just (TokOperator "\"", BSL.tail bs')
           116 -> readKeyword bs' "true" PdfBool True
           102 -> readKeyword bs' "false" PdfBool False
           110 -> readKeyword bs' "null" (const PdfNull) ()
           d   | isDigit (w2c d) -> readNumber bs'
           _   -> readOperator bs'

readKeyword :: BSL.ByteString -> String -> (a -> Obj) -> a -> Maybe (Token, BSL.ByteString)
readKeyword bs kw mk val =
  let k = BSLC.pack kw
      n = fromIntegral (length kw)
  in if BSL.take n bs == k && keywordEnd8 (BSL.drop n bs)
     then Just (TokOperand (mk val), BSL.drop n bs)
     else readOperator bs

readNumber :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readNumber bs =
  let (numBs, rest) = spanNum8 bs
  in if BSL.null numBs
     then Nothing
     else Just (TokOperand (PdfNumber (parsePdfNumber (map w2c (BSL.unpack numBs)))), rest)

spanNum8 :: BSL.ByteString -> (BSL.ByteString, BSL.ByteString)
spanNum8 bs =
  let (signed, rest) = case BSL.uncons bs of
        Just (45, r) -> (BSL.cons 45 BSL.empty, r)
        Just (43, r) -> (BSL.empty, r)
        _            -> (BSL.empty, bs)
      (intp, rest') = BSL.span isDigit8 (if BSL.null signed then bs else rest)
      start = if BSL.null signed then BSL.empty else signed
      (frac, rest'') = case BSL.uncons rest' of
        Just (46, r) ->
          let (ds, r') = BSL.span isDigit8 r
          in if BSL.null ds && BSL.null intp
             then (BSL.empty, bs)
             else (BSL.cons 46 ds, r')
        _ -> (BSL.empty, rest')
      num = start <> intp <> frac
  in if BSL.null num && not (BSL.take 1 frac == BSLC.pack ".")
     then (BSL.empty, bs)
     else (num, if BSL.null frac then rest' else rest'')

isDigit8 :: Word8 -> Bool
isDigit8 w = w >= 48 && w <= 57

isOpChar8 :: Word8 -> Bool
isOpChar8 w =
  let c = w2c w
  in (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c == '*'

normalizePdfNumber :: String -> String
normalizePdfNumber s
  | null s = s
  | head s == '.' = '0' : s
  | length s >= 2 && head s == '-' && s !! 1 == '.' = '-' : '0' : drop 2 s
  | otherwise = s

parsePdfNumber :: String -> Double
parsePdfNumber s
  | null s || s == "-" || s == "+" = 0
  | last s == '.' =
      case reads (normalizePdfNumber s ++ "0") of
        [(n, "")] -> n
        _         -> 0
  | otherwise =
      case reads (normalizePdfNumber s) of
        [(n, "")] -> n
        _         -> 0

readName :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readName bs =
  let body = BSL.dropWhile (not . nameEnd8) (BSL.tail bs)
      len = BSL.length bs - BSL.length body
  in if len > 1
     then Just (TokOperand (PdfName (T.pack (map w2c (BSL.unpack (BSL.take len bs))))), body)
     else Nothing

readLiteral :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readLiteral bs =
  case parseLiteral (map w2c (BSL.unpack (BSL.tail bs))) 1 [] of
    Nothing -> Nothing
    Just (bytes, rest) -> Just (TokOperand (PdfText (T.pack (map chr bytes))), BSLC.pack rest)

parseLiteral :: String -> Int -> [Int] -> Maybe ([Int], String)
parseLiteral [] _ _ = Nothing
parseLiteral s@(')' : rest) 1 acc = Just (reverse acc, rest)
parseLiteral ('\\' : ')' : rest) depth acc = parseLiteral rest depth (ord ')' : acc)
parseLiteral ('\\' : '(' : rest) depth acc = parseLiteral rest depth (ord '(' : acc)
parseLiteral ('\\' : 'n' : rest) depth acc = parseLiteral rest depth (ord '\n' : acc)
parseLiteral ('\\' : 'r' : rest) depth acc = parseLiteral rest depth (ord '\r' : acc)
parseLiteral ('\\' : 't' : rest) depth acc = parseLiteral rest depth (ord '\t' : acc)
parseLiteral ('\\' : 'b' : rest) depth acc = parseLiteral rest depth (ord '\b' : acc)
parseLiteral ('\\' : 'f' : rest) depth acc = parseLiteral rest depth (ord '\f' : acc)
parseLiteral ('\\' : '\\' : rest) depth acc = parseLiteral rest depth (ord '\\' : acc)
parseLiteral ('\\' : d : rest) depth acc
  | d `elem` ("01234567" :: String) =
      let (oct, rest') = span (`elem` ("01234567" :: String)) (d : rest)
          oct3 = take 3 oct
          val = case Num.readOct oct3 of
            [(n, "")] -> n
            _         -> ord '?'
      in parseLiteral rest' depth (val : acc)
parseLiteral ('\\' : _ : rest) depth acc = parseLiteral rest depth (ord '?' : acc)
parseLiteral ('(' : rest) depth acc = parseLiteral rest (depth + 1) acc
parseLiteral (c : rest) depth acc = parseLiteral rest depth (ord c : acc)

readHexString :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readHexString bs =
  case BSL.break ((== 62) . fromIntegral) (BSL.tail bs) of
    (hexBody, rest) ->
      case BSL.uncons rest of
        Just (_, r) ->
          let hex = BSL.filter hexDigit8 hexBody
          in if BSL.null hex
             then Nothing
             else Just (TokOperand (PdfHex (T.pack (map w2c (BSL.unpack hex)))), r)
        _ -> Nothing

readArray :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readArray bs = parseArrayElems (BSL.tail bs) []

parseArrayElems :: BSL.ByteString -> [Obj] -> Maybe (Token, BSL.ByteString)
parseArrayElems bs acc =
  let bs' = skipWs bs
  in case BSL.uncons bs' of
       Just (93, rest) -> Just (TokOperand (PdfArray (reverse acc)), rest)
       _ -> case readToken bs' of
         Just (TokOperand o, rest) -> parseArrayElems rest (o : acc)
         _ -> Nothing

readDict :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readDict bs = parseDictPairs (BSL.drop 2 bs) M.empty

parseDictPairs :: BSL.ByteString -> Dict -> Maybe (Token, BSL.ByteString)
parseDictPairs bs acc =
  let bs' = skipWs bs
  in case BSL.take 2 bs' of
       k | k == BSLC.pack ">>" ->
         Just (TokOperand (PdfDict acc), BSL.drop 2 bs')
       _ -> case readToken bs' of
         Just (TokOperand (PdfName key), rest1) ->
           case readToken (skipWs rest1) of
             Just (TokOperand val, rest2) -> parseDictPairs rest2 (M.insert key val acc)
             _ -> Nothing
         _ -> Nothing

readOperator :: BSL.ByteString -> Maybe (Token, BSL.ByteString)
readOperator bs =
  let (opBs, rest) = BSL.span isOpChar8 bs
  in if BSL.null opBs
     then Nothing
     else Just (TokOperator (map w2c (BSL.unpack opBs)), rest)

skipInlineImage :: BSL.ByteString -> BSL.ByteString
skipInlineImage bs =
  let afterID = skipToWsKeyword bs "ID"
  in skipToWsKeyword afterID "EI"

skipToWsKeyword :: BSL.ByteString -> String -> BSL.ByteString
skipToWsKeyword bs kw =
  let n = fromIntegral (length kw)
      lim = max 0 (BSL.length bs - n)
      kwBs = BSLC.pack kw
  in case findMatch n kwBs 0 lim of
       Nothing -> BSL.empty
       Just i ->
         let after = BSL.drop (i + 1 + n) bs
         in skipWs after
  where
    findMatch n kwBs i lim
      | i > lim = Nothing
      | otherwise =
          let w = BSL.index bs i
          in if isWs8 w && BSL.take n (BSL.drop (i + 1) bs) == kwBs
             then Just i
             else findMatch n kwBs (i + 1) lim

findDict :: [Obj] -> Maybe Dict
findDict objs = case find isDict objs of
  Just (PdfDict d) -> Just d
  _ -> Nothing
  where
    isDict (PdfDict _) = True
    isDict _           = False
