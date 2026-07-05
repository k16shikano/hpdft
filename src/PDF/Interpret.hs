{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module PDF.Interpret
  ( Glyph(..)
  , interpretPage
  , interpretContent
  , interpretContentWithFonts
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
  , rawStreamByRef
  )
import PDF.Matrix
import PDF.Character (pdfcharmap, adobeJapanOneSixMap)
import PDF.Encrypt (Security)
import PDF.Error (PdfError(..), PdfResult)
import PDF.Object (parseRefsArray)

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Char (chr, ord)
import Data.List (find, foldl', isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import Numeric (readHex, readOct)
import Text.Parsec hiding ((<|>), updateState)
import Text.Parsec.ByteString.Lazy

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
  , glyphFont     :: String
  , glyphWMode    :: Int
  } deriving (Eq, Show)

data GState = GState
  { ctm        :: Matrix
  , gsFontRes  :: Maybe String
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
  { gsCur      :: GState
  , gsStack    :: [GState]
  , tsCur      :: Maybe TState
  , glyphsAcc  :: [Glyph] -> [Glyph]
  , depth      :: Int
  , isSec      :: Maybe Security
  , isObjs     :: PDFObjIndex
  , isRes      :: Dict
  , fontOverrides :: M.Map String FontInfo
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

initialIState :: Maybe Security -> PDFObjIndex -> Dict -> IState
initialIState sec objs res = IState
  { gsCur = initialGState
  , gsStack = []
  , tsCur = Nothing
  , glyphsAcc = id
  , depth = 0
  , isSec = sec
  , isObjs = objs
  , isRes = res
  , fontOverrides = M.empty
  }

maxFormDepth :: Int
maxFormDepth = 12

interpretContent :: Maybe Security -> PDFObjIndex -> Dict -> BSL.ByteString -> [Glyph]
interpretContent sec objs res bytes =
  interpretContentWithFonts sec objs res M.empty bytes

interpretContentWithFonts :: Maybe Security -> PDFObjIndex -> Dict -> M.Map String FontInfo -> BSL.ByteString -> [Glyph]
interpretContentWithFonts sec objs res fonts bytes =
  let st0 = initialIState sec objs res
      st1 = st0 {fontOverrides = fonts}
  in case runParser interpretEnd st1 "" bytes of
    Left _  -> []
    Right st -> reverse (glyphsAcc st [])

interpretEnd :: ISParser IState
interpretEnd = do
  spaces
  many contentOp
  spaces
  eof
  getState

interpretPage :: Document -> Int -> PdfResult [Glyph]
interpretPage doc pageRef = do
  pageDict <- case findObjsByRef pageRef (docObjs doc) of
    Just os -> case findDictOfType "/Page" os of
      Just d -> Right d
      Nothing -> Left (MissingKey "/Type" ("page " ++ show pageRef))
    Nothing -> Left (MissingObject pageRef)
  res <- case pageResourcesInherited pageDict (docObjs doc) of
    Just r -> Right r
    Nothing -> Right M.empty
  content <- pageContentsBytes (docSecurity doc) (docObjs doc) pageDict
  return $ interpretContent (docSecurity doc) (docObjs doc) res content

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

pageContentsBytes :: Maybe Security -> PDFObjIndex -> Dict -> PdfResult BSL.ByteString
pageContentsBytes sec objs dict = case M.lookup "/Contents" dict of
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
    singleStream ref = rawStreamByRef sec objs ref

type ISParser a = GenParser Char IState a

contentOp :: ISParser ()
contentOp =
  choice
    [ try textObject
    , try graphicsStateOp
    , try textStateOp
    , try textPositionOp
    , try textShowOp
    , try xObjectOp
    , try inlineImageOp
    , try ignoredPathPaintOp
    , try ignoredColorOp
    , try ignoredMarkedContentOp
    , skipUnknownLine
    ]

textObject :: ISParser ()
textObject = do
  spaces
  choice
    [ try $ string "BT" >> spaces >> withTextState (manyTill contentOp (try (string "ET"))) >> spaces
    , try $ string "ET" >> spaces >> modifyIState (\s -> s {tsCur = Nothing})
    ]

withTextState :: ISParser a -> ISParser a
withTextState p = do
  modifyIState (\s -> s {tsCur = Just (TState identity identity)})
  p

graphicsStateOp :: ISParser ()
graphicsStateOp = do
  spaces
  choice
    [ try $ string "q" >> spaces >> pushGState
    , try $ string "Q" >> spaces >> popGState
    , try $ do
        a <- digitParam
        spaces
        b <- digitParam
        spaces
        c <- digitParam
        spaces
        d <- digitParam
        spaces
        e <- digitParam
        spaces
        f <- digitParam
        spaces
        string "cm"
        spaces
        let cmMat = mkMatrix a b c d e f
        modifyGState (\gs -> gs {ctm = multiply cmMat (ctm gs)})
    ]

pushGState :: ISParser ()
pushGState = modifyIState (\s -> s {gsStack = gsCur s : gsStack s})

popGState :: ISParser ()
popGState = modifyIState $ \s ->
  case gsStack s of
    (g:gs) -> s {gsCur = g, gsStack = gs}
    []     -> s

textStateOp :: ISParser ()
textStateOp = do
  spaces
  choice
    [ try $ do
        font <- pdfName
        spaces
        size <- digitParam
        spaces
        string "Tf"
        spaces
        resolveFont font size
    , try $ setGSDouble "Tc" (\v gs -> gs {gsCharSp = v})
    , try $ setGSDouble "Tw" (\v gs -> gs {gsWordSp = v})
    , try $ do
        v <- digitParam
        spaces
        string "Tz"
        spaces
        modifyGState (\gs -> gs {gsHScale = v / 100})
    , try $ setGSDouble "TL" (\v gs -> gs {gsLeading = v})
    , try $ setGSDouble "Ts" (\v gs -> gs {gsRise = v})
    , try $ setGSInt "Tr" (\v gs -> gs {gsRender = v})
    ]

setGSDouble :: String -> (Double -> GState -> GState) -> ISParser ()
setGSDouble op f = do
  v <- digitParam
  spaces
  string op
  spaces
  modifyGState (f v)

setGSInt :: String -> (Int -> GState -> GState) -> ISParser ()
setGSInt op f = do
  v <- digitParam
  spaces
  string op
  spaces
  modifyGState (f (truncate v))

resolveFont :: String -> Double -> ISParser ()
resolveFont fontName size = do
  st <- getState
  let fi = lookupFont (isSec st) (isObjs st) (isRes st) fontName st
  modifyGState (\gs -> gs {gsFontRes = Just fontName, gsFont = fi, gsFontSize = size})

lookupFont sec objs res fontName st =
  case M.lookup fontName (fontOverrides st) of
    Just fi -> Just fi
    Nothing -> lookupFontResource sec objs res fontName

lookupFontResource :: Maybe Security -> PDFObjIndex -> Dict -> String -> Maybe FontInfo
lookupFontResource sec objs res fontName =
  case findObjFromDict res "/Font" of
    Just (PdfDict fd) -> fontFromDict sec objs fd fontName
    Just (ObjRef r) ->
      case findDictByRef r objs of
        Just fd -> fontFromDict sec objs fd fontName
        Nothing -> Nothing
    _ -> Nothing

fontFromDict :: Maybe Security -> PDFObjIndex -> Dict -> String -> Maybe FontInfo
fontFromDict sec objs fd name =
  case M.lookup name fd of
    Just (ObjRef r) -> Just (fontInfo sec r objs)
    _ -> Nothing

textPositionOp :: ISParser ()
textPositionOp = do
  spaces
  choice
    [ try $ do
        tx <- digitParam
        spaces
        ty <- digitParam
        spaces
        string "Td"
        spaces
        textTd tx ty
    , try $ do
        tx <- digitParam
        spaces
        ty <- digitParam
        spaces
        string "TD"
        spaces
        modifyGState (\gs -> gs {gsLeading = -ty})
        textTd tx ty
    , try $ do
        a <- digitParam
        spaces
        b <- digitParam
        spaces
        c <- digitParam
        spaces
        d <- digitParam
        spaces
        e <- digitParam
        spaces
        f <- digitParam
        spaces
        string "Tm"
        spaces
        textSetMatrix (mkMatrix a b c d e f)
    , try $ string "T*" >> spaces >> withTextMatrix textLeadingNewline
    ]

textTd :: Double -> Double -> ISParser ()
textTd tx ty = withTextMatrix $ do
  st <- getState
  case tsCur st of
    Nothing -> return ()
    Just ts ->
      let tlm' = multiply (translate tx ty) (tlmMat ts)
      in modifyIState (\s -> s {tsCur = Just (TState {tmMat = tlm', tlmMat = tlm'})})

textSetMatrix :: Matrix -> ISParser ()
textSetMatrix m = modifyIState (\s -> s {tsCur = Just (TState {tmMat = m, tlmMat = m})})

textLeadingNewline :: ISParser ()
textLeadingNewline = do
  st <- getState
  let leading = -(gsLeading (gsCur st))
  textTd 0 leading

withTextMatrix :: ISParser () -> ISParser ()
withTextMatrix p = do
  st <- getState
  if isJust (tsCur st) then p else return ()

textShowOp :: ISParser ()
textShowOp = do
  spaces
  choice
    [ try $ do
        bytes <- stringOperand
        spaces
        string "Tj"
        spaces
        showBytes bytes
    , try $ do
        elems <- tjArray
        spaces
        string "TJ"
        spaces
        showTJ elems
    , try $ do
        string "'"
        spaces
        withTextMatrix textLeadingNewline
        bytes <- stringOperand
        spaces
        showBytes bytes
    , try $ do
        aw <- digitParam
        spaces
        ac <- digitParam
        spaces
        bytes <- stringOperand
        spaces
        string "\""
        spaces
        modifyGState (\gs -> gs {gsWordSp = aw, gsCharSp = ac})
        withTextMatrix textLeadingNewline
        showBytes bytes
    ]

stringOperand :: ISParser [Int]
stringOperand =
  choice [ try hexStringBytes, try literalStringBytes ]

tjArray :: ISParser [TJElem]
tjArray = do
  char '['
  spaces
  elems <- many tjElem
  char ']'
  return elems

data TJElem = TJString [Int] | TJAdjust Double

tjElem :: ISParser TJElem
tjElem =
  choice
    [ TJAdjust <$> (try digitParam <* spaces)
    , TJString <$> (try stringOperand <* spaces)
    ]

showTJ :: [TJElem] -> ISParser ()
showTJ elems = mapM_ go elems
  where
    go (TJString bs) = showBytes bs
    go (TJAdjust k)  = tjKern k

tjKern :: Double -> ISParser ()
tjKern k = withTextMatrix $ do
  st <- getState
  let gs = gsCur st
      fi = gsFont gs
      wmode = maybe 0 fiWMode fi
      disp = -k / 1000 * gsFontSize gs * gsHScale gs
  case tsCur st of
    Nothing -> return ()
    Just ts ->
      let tm' = if wmode == 1
                then multiply (translate 0 disp) (tmMat ts)
                else multiply (translate disp 0) (tmMat ts)
      in modifyIState (\s -> s {tsCur = Just ts {tmMat = tm'}})

showBytes :: [Int] -> ISParser ()
showBytes bytes = withTextMatrix $ do
  st <- getState
  case (tsCur st, gsFont (gsCur st), gsFontRes (gsCur st)) of
    (Nothing, _, _) -> return ()
    (_, Nothing, _) -> return ()
    (Just ts, Just fi, Just fname) -> do
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
            }
      modifyIState (\s -> s {glyphsAcc = (glyph :) . glyphsAcc s, tsCur = Just ts {tmMat = endTm}})
    _ -> return ()

glyphStep :: GState -> FontInfo -> (T.Text, Matrix) -> Int -> (T.Text, Matrix)
glyphStep gs fi (txt, tm) code =
  let u = codeToUnicode fi code
      (tx, ty) = codeAdvance gs fi code
      tm' = multiply (translate tx ty) tm
  in (txt `T.append` u, tm')

bytesToCodes :: FontInfo -> [Int] -> [Int]
bytesToCodes fi bytes =
  if fiBytesPerCode fi == 2
  then pairs bytes
  else bytes
  where
    pairs [] = []
    pairs [_] = []
    pairs (a:b:rest) = (a * 256 + b) : pairs rest

codeToUnicode :: FontInfo -> Int -> T.Text
codeToUnicode fi code =
  case M.lookup code (fiToUnicode fi) of
    Just s -> T.pack s
    Nothing -> encodingUnicode (fiEncoding fi) code

encodingUnicode :: Encoding -> Int -> T.Text
encodingUnicode (Encoding enc) code =
  case M.lookup (chr code) enc of
    Just glyph ->
      case M.lookup glyph pdfcharmap of
        Just u -> u
        Nothing -> if "/uni" `isPrefixOf` glyph
                   then readUniGlyph glyph
                   else T.pack glyph
    Nothing -> T.singleton (safeChr code)
encodingUnicode (CIDmap "Adobe-Japan1") code =
  case M.lookup code adobeJapanOneSixMap of
    Just bs -> T.pack (BSLU.toString bs)
    Nothing -> T.singleton (safeChr code)
encodingUnicode (CIDmap _) code = T.singleton (safeChr code)
encodingUnicode WithCharSet{} code = T.singleton (safeChr code)
encodingUnicode NullMap code = T.singleton (safeChr code)

readUniGlyph :: String -> T.Text
readUniGlyph s =
  case readHex (drop 4 s) of
    [(i, "")] -> T.singleton (chr i)
    _         -> T.pack s

safeChr :: Int -> Char
safeChr n
  | n >= 0 && n <= 0x10FFFF = chr n
  | otherwise = '\xfffd'

codeAdvance :: GState -> FontInfo -> Int -> (Double, Double)
codeAdvance gs fi code =
  let tfs = gsFontSize gs
      tc = gsCharSp gs
      tw = gsWordSp gs
      th = gsHScale gs
      w0 = fiWidth fi code / 1000
  in if fiWMode fi == 1
     then (0, (fiWidthV fi code / 1000) * tfs + tc + tw)
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

xObjectOp :: ISParser ()
xObjectOp = do
  name <- pdfName
  spaces
  string "Do"
  spaces
  invokeXObject name

invokeXObject :: String -> ISParser ()
invokeXObject name = do
  st <- getState
  case findObjFromDict (isRes st) "/XObject" of
    Just (PdfDict xd) -> case M.lookup name xd of
      Just (ObjRef r) -> runXObject r
      _ -> return ()
    Just (ObjRef xr) ->
      case findDictByRef xr (isObjs st) of
        Just xd -> case M.lookup name xd of
          Just (ObjRef r) -> runXObject r
          _ -> return ()
        Nothing -> return ()
    _ -> return ()

runXObject :: Int -> ISParser ()
runXObject ref = do
  st0 <- getState
  when (depth st0 >= maxFormDepth) $ return ()
  case findObjsByRef ref (isObjs st0) of
    Just os -> case findDict os of
      Just d ->
        case M.lookup "/Subtype" d of
          Just (PdfName "/Form") -> do
            stream <- case rawStreamByRef (isSec st0) (isObjs st0) ref of
              Right s -> return s
              Left _  -> return BSL.empty
            let formMat = formMatrix d
                formRes = fromMaybe (isRes st0) (findResourcesDict d (isObjs st0))
            pushGState
            modifyGState (\gs -> gs {ctm = multiply formMat (ctm gs)})
            stMid <- getState
            let stRun = stMid {isRes = formRes, depth = depth st0 + 1}
            stDone <- case runParser formEnd stRun "" stream of
              Right st -> return st
              Left _ -> return stRun
            popGState
            modifyIState (\s -> s {glyphsAcc = glyphsAcc stDone, depth = depth st0, isRes = isRes st0})
          _ -> return ()
      Nothing -> return ()
    Nothing -> return ()

formMatrix :: Dict -> Matrix
formMatrix d = case M.lookup "/Matrix" d of
  Just (PdfArray [PdfNumber a, PdfNumber b, PdfNumber c, PdfNumber d', PdfNumber e, PdfNumber f]) ->
    mkMatrix a b c d' e f
  _ -> identity

inlineImageOp :: ISParser ()
inlineImageOp = do
  spaces
  string "BI"
  manyTill anyChar (try $ oneOf " \t\r\n" >> string "ID")
  oneOf " \t\r\n"
  skipTillEI

skipTillEI :: ISParser ()
skipTillEI = go
  where
    go = do
      eof <|> try (do
        _ <- oneOf " \t\r\n"
        string "EI"
        optional (oneOf " \t\r\n")
        ) <|> do
        _ <- anyChar
        go

ignoredPathPaintOp :: ISParser ()
ignoredPathPaintOp = do
  spaces
  choice
    [ try $ oneOf "qQ" >> spaces
    , try $ oneOf "fFbBW" >> optional (string "*") >> spaces
    , try $ oneOf "nsS" >> spaces
    , try $ digitParam >> spaces >> oneOf "jJM" >> spaces
    , try $ digitParam >> spaces >> oneOf "dwi" >> spaces
    , try $ many (digitParam >> spaces) >> oneOf "ml" >> spaces
    , try $ many (digitParam >> spaces) >> oneOf "vy" >> spaces
    , try $ many (digitParam >> spaces) >> string "re" >> spaces
    , try $ many (digitParam >> spaces) >> oneOf "c" >> spaces
    , try $ oneOf "h" >> spaces
    , try $ pdfName >> spaces >> string "gs" >> spaces
    , try $ char '[' >> many digitParam >> char ']' >> spaces >> many1 digitParam >> spaces >> string "d" >> spaces
    , try $ many (digitParam >> spaces) >> string "sh" >> spaces
    ]

ignoredColorOp :: ISParser ()
ignoredColorOp = do
  spaces
  choice
    [ try $ pdfName >> spaces >> (string "CS" <|> string "cs") >> spaces
    , try $ many (digitParam >> spaces) >> string "rg" >> spaces
    , try $ many (digitParam >> spaces) >> string "RG" >> spaces
    , try $ digitParam >> spaces >> oneOf "gG" >> spaces
    , try $ many (digitParam >> spaces) >> oneOf "kK" >> spaces
    , try $ many (digitParam >> spaces) >> string "SCN" >> spaces
    , try $ many (digitParam >> spaces) >> string "scn" >> spaces
    , try $ many (digitParam >> spaces) >> string "SC" >> spaces
    , try $ many (digitParam >> spaces) >> string "sc" >> spaces
    , try $ pdfName >> spaces >> string "ri" >> spaces
    , try $ pdfName >> spaces >> string "Intent" >> spaces
    ]

ignoredMarkedContentOp :: ISParser ()
ignoredMarkedContentOp = do
  spaces
  choice
    [ try $ pdfName >> spaces >> string "BMC" >> spaces >> manyTill contentOp (try (string "EMC")) >> spaces
    , try $ pdfName >> propertyDict >> spaces >> string "BDC" >> spaces >> manyTill contentOp (try (string "EMC")) >> spaces
    , try $ pdfName >> spaces >> string "EMC" >> spaces
    , try $ pdfName >> propertyDict >> spaces >> string "DP" >> spaces
    , try $ pdfName >> spaces >> string "MP" >> spaces
    ]

formEnd :: ISParser IState
formEnd = do
  spaces
  many contentOp
  spaces
  getState

propertyDict :: ISParser ()
propertyDict = do
  spaces
  string "<<"
  _ <- manyTill anyChar (try (string ">>"))
  return ()

skipUnknownLine :: ISParser ()
skipUnknownLine = do
  c <- lookAhead anyChar
  if c `elem` (' ':['\t', '\r', '\n'])
    then fail "not unknown"
    else many1 (noneOf " \t\r\n") >> return ()

pdfName :: ISParser String
pdfName = (++) <$> string "/" <*> manyTill anyChar (try $ lookAhead $ oneOf " \t\r\n/[(")

literalStringBytes :: ISParser [Int]
literalStringBytes = do
  char '('
  bs <- manyTill stringChar (char ')')
  spaces
  return bs

stringChar :: ISParser Int
stringChar =
  choice
    [ try $ char '\\' >> char ')' >> return (ord ')')
    , try $ char '\\' >> char '(' >> return (ord '(')
    , try $ char '\\' >> char 'n' >> return (ord '\n')
    , try $ char '\\' >> char 'r' >> return (ord '\r')
    , try $ char '\\' >> char 't' >> return (ord '\t')
    , try $ char '\\' >> char 'b' >> return (ord '\b')
    , try $ char '\\' >> char 'f' >> return (ord '\f')
    , try $ char '\\' >> char '\\' >> return (ord '\\')
    , try $ octEscape
    , ord <$> noneOf "\\)"
    ]

octEscape :: ISParser Int
octEscape = do
  char '\\'
  ds <- count 3 (oneOf "01234567")
  case readOct ds of
    [(n, "")] -> return n
    _         -> return (ord '?')

hexStringBytes :: ISParser [Int]
hexStringBytes = do
  char '<'
  hex <- many (oneOf "0123456789abcdefABCDEF")
  char '>'
  spaces
  return (hexPairs hex)

hexPairs :: String -> [Int]
hexPairs [] = []
hexPairs [x] =
  case readHex [x, '0'] of
    [(n, "")] -> [n]
    _         -> []
hexPairs (a:b:rest) =
  case readHex [a, b] of
    [(n, "")] -> n : hexPairs rest
    _         -> hexPairs rest

digitParam :: ISParser Double
digitParam = do
  sign <- option "" (char '-' >> return "-")
  num <- (try ((string "0" <|> string "") >> char '.' >> many1 digit >>= \ds -> return ('0':'.':ds)))
         <|> (many1 digit >>= \ds -> option "" (char '.' >> many digit) >>= \fr -> return (ds ++ fr))
  return $ parsePdfNumber (sign ++ num)

parsePdfNumber :: String -> Double
parsePdfNumber s
  | null s || s == "-" = 0
  | last s == '.' = read (s ++ "0")
  | otherwise = read s

modifyGState :: (GState -> GState) -> ISParser ()
modifyGState f = modifyIState (\s -> s {gsCur = f (gsCur s)})

modifyIState :: (IState -> IState) -> ISParser ()
modifyIState f = modifyState f

findDict :: [Obj] -> Maybe Dict
findDict objs = case find isDict objs of
  Just (PdfDict d) -> Just d
  _ -> Nothing
  where
    isDict (PdfDict _) = True
    isDict _           = False
