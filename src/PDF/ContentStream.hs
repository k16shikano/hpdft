{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module PDF.ContentStream 
       ( parseStream
       , parseColorSpace
       ) where

import PDF.StreamLex
  ( hexPairs
  , parsePdfNumber
  , sjisBytesToCodes
  , unicodeBytesToCodes
  , jisBytesToCodes
  )

import Data.Char (chr, ord)
import Data.String (fromString)
import Data.List (isPrefixOf, dropWhileEnd)
import Numeric (readOct, readHex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import GHC.Word (Word8)
import qualified Data.ByteString as B (pack)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSSC (unpack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (ByteString, pack)
import qualified Data.ByteString.Lazy.UTF8 as BSLU (toString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf16BE)

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.ByteString.Lazy
import Control.Applicative

import PDF.Definition
import PDF.Object
import PDF.Character (pdfcharmap, extendedAscii, adobeJapanOneSixMap, cp932Map, jisx0208Map)
import PDF.Error (PdfError(..), PdfResult, PdfWarning(..))

type PSParser a = GenParser Char PSR a

parseContentStream p st = runParser p st ""

type FormRunner = T.Text -> PSR -> T.Text

noFormRunner :: FormRunner
noFormRunner _ _ = T.empty

parseStream :: FormRunner -> PSR -> PDFStream -> PdfResult (PDFStream, [PdfWarning])
parseStream formRunner psr pdfstream =
  case parseContentStream (contentParser formRunner) psr pdfstream of
    Left err -> Left (ParseError ("content stream: " ++ show err) BS.empty)
    Right (str, ws) -> Right (BSLC.pack $ BSSC.unpack $ encodeUtf8 str, reverse ws)
  where
    contentParser runner = do
      str <- T.concat <$> (spaces >> many (try (elems runner) <|> skipOther))
      st <- getState
      return (str, warnings st)

parseColorSpace :: PSR -> BSLC.ByteString -> PdfResult [T.Text]
parseColorSpace psr pdfstream = 
  case parseContentStream (many (choice [ try colorSpace
                                        , try $ T.concat <$> xObject
                                        , (T.empty <$ elems noFormRunner)
                                        ])) psr pdfstream of
    Left  err -> Left (ParseError ("color space: " ++ show err) BS.empty)
    Right str -> Right str


-- | Parsers for Content Stream

elems :: FormRunner -> PSParser T.Text
elems formRunner = choice [ try (pdfopBT formRunner)
               , try pdfopTf
               , try pdfopTD
               , try pdfopTd
               , try pdfopTm
               , try pdfopTc
               , try pdfopTs
               , try pdfopTw
               , try pdfopTL
               , try pdfopTz
               , try pdfopTj
               , try pdfopTJ
               , try pdfopTr
               , try pdfQuote
               , try pdfDoubleQuote
               , try pdfopTast
               , try letters <* spaces
               , try hexletters <* spaces
               , try array <* spaces
               , try pdfopGraphics
               , try dashPattern
               , try (formDoOp formRunner)
               , try graphicState
               , try pdfopcm
               , try $ T.empty <$ colorSpace
               , try $ T.empty <$ renderingIntent
               , try (pdfopBDC formRunner)
               , try (pdfopBMC formRunner)
               , try pdfopEMC
               , unknowns
               ]

formDoOp :: FormRunner -> PSParser T.Text
formDoOp runner = do
  n <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  string "Do"
  spaces
  st <- getState
  return $ runner (T.pack n) st

pdfopGraphics :: PSParser T.Text
pdfopGraphics = do
  spaces
  choice [ try $ T.empty <$ oneOf "qQ" <* spaces
         , try $ T.empty <$ oneOf "fFbBW" <* (many $ string "*") <* space <* spaces
         , try $ T.empty <$ oneOf "nsS" <* spaces
         , try $ T.empty <$ (digitParam <* spaces) <* oneOf "jJM" <* space <* spaces
         , try $ T.empty <$ (digitParam <* spaces) <* oneOf "dwi" <* spaces
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* oneOf "ml" <* space <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* oneOf "vy" <* space <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* string "re" <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* string "SCN" <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* string "scn" <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* string "SC" <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* string "sc" <* spaces)
         , try $ T.empty <$ (many1 (digitParam <* spaces) <* string "c" <* space <* spaces)
         , try $ T.empty <$ oneOf "h" <* spaces         
         ]
  return T.empty

graphicState :: PSParser T.Text
graphicState = do
  gs <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  string "gs"
  spaces
  return T.empty

colorSpace :: PSParser T.Text
colorSpace = do
  gs <- choice [ try $ string "/" *> manyTill anyChar (try space) <* (string "CS" <|> string "cs") <* spaces
               , try $ "DeviceRGB" <$ (many1 (digitParam <* spaces) <* string "rg" <* spaces)
               , try $ "DeviceRGB" <$ (many1 (digitParam <* spaces) <* string "RG" <* spaces)
               , try $ "DeviceGray" <$ (digitParam <* spaces) <* oneOf "gG" <* spaces
               , try $ "DeviceCMYK" <$ (many1 (digitParam <* spaces) <* oneOf "kK" <* spaces)
               ] 
  updateState (\s -> s {colorspace = T.pack gs})
  return $ T.pack gs

dashPattern :: PSParser T.Text
dashPattern = do
  char '[' >> many digit >> char ']' >> spaces >> many1 digit >> spaces >> string "d"
  return T.empty

renderingIntent :: PSParser T.Text
renderingIntent = do
  ri <- choice [ try $ string "/" *> manyTill anyChar (try space) <* string "ri" <* spaces
               , try $ string "/" *> manyTill anyChar (try space) <* string "Intent" <* spaces
               ]
  return $ T.pack ri

xObject :: PSParser [T.Text]
xObject = do
  n <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  string "Do"
  spaces
  st <- getState
  let xobjcs = xcolorspaces st
--  updateState (\s -> s {colorspace = xobjcs})
  return xobjcs

pdfopBT :: FormRunner -> PSParser T.Text
pdfopBT formRunner = do
  st <- getState
  updateState (\s -> s{text_m = (1,0,0,1,0,0), text_break = False})
  string "BT"
  spaces
  t <- manyTill (elems formRunner) (try $ string "ET")
  spaces
  return $ T.concat t

-- should have refined according to the section 10.5 of PDF reference

pdfopBMC :: FormRunner -> PSParser T.Text
pdfopBMC formRunner = do
  tag <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  string "BMC"
  spaces
  manyTill (elems formRunner) (try $ string "EMC")
  spaces
  return T.empty

pdfopBDC :: FormRunner -> PSParser T.Text
pdfopBDC formRunner = do
  tag <- name
  prop <- propertyList
  spaces
  string "BDC"
  spaces
  case tag of
    "/Span" 
      | "/ActualText" == (fst prop)
        -> do {spaces >> manyTill (elems formRunner) (try $ string "EMC") >> return (snd prop)}
      | otherwise  -> return $ T.empty
    _ -> return $ T.empty

  where
    propertyList :: PSParser (T.Text, T.Text)
    propertyList = spaces >> try dictionary

    dictionary :: PSParser (T.Text, T.Text)
    dictionary = do
      _ <- spaces >> string "<<" >> spaces
      name <- name
      entries <- T.concat <$> manyTill dictEntry (try (string ">>" >> (notFollowedBy $ string ">")))
      return (name, entries)

    dictEntry :: PSParser T.Text
    dictEntry = choice [ try name
                       , try letters
                       , hexDecodeUTF16BE . T.pack <$> try hex
                       , T.pack <$> try (many1 digit)
                       ] <* spaces

    hex = string "<" >> (manyTill (oneOf "0123456789abcdefABCDEF") (try $ string ">"))

    name :: PSParser T.Text
    name = T.pack <$>
           ((++) <$> string "/"
             <*> (manyTill anyChar (try $ lookAhead $ oneOf "><][)( \n\r/")) <* spaces)

pdfopEMC :: PSParser T.Text
pdfopEMC = do
  spaces
  string "EMC"
  spaces
  return T.empty




pdfopTj :: PSParser T.Text
pdfopTj = do
  spaces
  t <- manyTill (letters <|> hexletters <|> array) (try $ string "Tj")
  spaces
  st <- getState
  let needBreak = text_break st
      t' = (if needBreak then ("\n":t) else t)
  updateState (\s -> s{text_break = False})
  return $ T.concat t'

pdfopTJ :: PSParser T.Text
pdfopTJ = do
  spaces
  t <- manyTill array (try $ string "TJ")
  spaces
  st <- getState
  let needBreak = text_break st
      t' = (if needBreak then ("":t) else t)
  updateState (\s -> s{text_break = False})
  return $ T.concat t'

pdfDoubleQuote :: PSParser T.Text
pdfDoubleQuote = do
  spaces
  t <- manyTill (letters <|> hexletters <|> array) (try $ string "\"")
  spaces
  return $ T.concat t
  
pdfQuote :: PSParser T.Text
pdfQuote = do
  spaces
  t <- manyTill (letters <|> hexletters <|> array) (try $ string "\'")
  spaces
  return $ T.concat t

unknowns :: PSParser T.Text
unknowns = do
  ps <- manyTill anyChar (try $ oneOf "\r\n")
  st <- getState
  case runParser (elems noFormRunner) st "" $ BSLC.pack ((Data.List.dropWhileEnd (=='\\') ps)++")Tj") of
    Right xs -> return xs
    Left _ -> case runParser (elems noFormRunner) st "" $ BSLC.pack ("("++ps) of
      Right xs -> return xs
      Left _ -> case ps of
        "" -> return T.empty
        _ -> do
          updateState (\s -> s {warnings = UnknownOperator (take 100 ps) : warnings s})
          return T.empty

skipOther :: PSParser T.Text
skipOther = do
  a <- manyTill anyChar (try $ oneOf "\r\n")
  return ""

array :: PSParser T.Text
array = do
  st <- getState
  char '['
  spaces
  str <- manyTill (letters <|> hexletters <|> kern) (try $ char ']')
  -- for TJ
  let needBreak = text_break st
      t' = (if needBreak then "\n":str else str)
  updateState (\s -> s{text_break = False})
  return $ T.concat t'

letters :: PSParser T.Text
letters = do
  char '('
  st <- getState
  let cmap = Map.findWithDefault Map.empty (curfont st) (cmaps st)
      letterParser = case Map.lookup (curfont st) (fontmaps st) of
        Just (Encoding m) -> psletter m
        Just (CIDmap s) -> cidletter s
        Just SJISmap -> sjisletters
        Just UnicodeMap -> unicodeletters
        Just JISmap -> jisletters
        Just (WithCharSet s) -> try $ bytesletter cmap <|> cidletters
        Just NullMap -> psletter Map.empty
        Nothing -> (T.pack) <$> (many1 $ choice [ try $ ')' <$ (string "\\)")
                                                , try $ '(' <$ (string "\\(")
                                                , try $ noneOf ")"
                                                ])
  lets <- manyTill letterParser $ (try $ char ')')
  spaces
  return $ T.concat lets

bytesletter :: CMap -> PSParser T.Text
bytesletter cmap = do
  txt <- (many1 $ choice [ try $ ')' <$ (string "\\)")
                         , try $ '(' <$ (string "\\(")
                         , try $ (chr 10) <$ (string "\\n")
                         , try $ (chr 13) <$ (string "\\r")
                         , try $ (chr 8) <$ (string "\\b")
                         , try $ (chr 9) <$ (string "\\t")
                         , try $ (chr 12) <$ (string "\\f")
                         , try $ (chr 92) <$ (string "\\\\")
                         , try $ (chr 0) <$ (char '\NUL')
                         , try $ (chr 32) <$ (char ' ')
                         , try $ chr <$> ((string "\\") *> octnum)
                         , try $ noneOf ")"
                         ])
  byteStringToText cmap txt
  where
    byteStringToText cmap' str = do
      parts <- mapM (lookupUcs Nothing cmap') $ asInt16 $ map ord str
      return $ T.concat parts

    asInt16 :: [Int] -> [Int]
    asInt16 [] = []
    asInt16 (a:[]) = [a]
    asInt16 (a:b:rest) = (a * 256 + b):(asInt16 rest)

hexletters :: PSParser T.Text
hexletters = do
  st <- getState
  char '<'
  hexChars <- many (oneOf "0123456789ABCDEFabcdef \t\r\n")
  char '>'
  spaces
  let enc = Map.lookup (curfont st) (fontmaps st)
      cmap = Map.findWithDefault Map.empty (curfont st) (cmaps st)
      codes = hexStringToCodes enc (filter (\c -> c `notElem` (" \t\r\n" :: String)) hexChars)
  parts <- mapM (lookupUcs enc cmap) codes
  return $ T.concat parts

-- | Split a hex string into character codes (matches 'PDF.Interpret.bytesToCodes').
hexStringToCodes :: Maybe Encoding -> String -> [Int]
hexStringToCodes enc hex =
  let bytes = hexPairs hex
  in case enc of
    Just SJISmap -> sjisBytesToCodes bytes
    Just UnicodeMap -> unicodeBytesToCodes bytes
    Just JISmap -> jisBytesToCodes bytes
    Just (CIDmap _) -> pairBytes bytes
    _ -> bytes
  where
    pairBytes [] = []
    pairBytes [_] = []
    pairBytes (a:b:rest) = (a * 256 + b) : pairBytes rest

sjisCodeToText :: Int -> T.Text
sjisCodeToText code =
  case Map.lookup code cp932Map of
    Just bs -> T.pack $ BSLU.toString bs
    Nothing ->
      if code >= 0 && code <= 0x7F
      then T.singleton (chr code)
      else "\xFFFD"

unicodeCodeToText :: Int -> T.Text
unicodeCodeToText code =
  if code >= 0 && code <= 0x10FFFF
  then T.singleton (chr code)
  else "\xFFFD"

jisCodeToText :: Int -> T.Text
jisCodeToText code =
  case Map.lookup code jisx0208Map of
    Just bs -> T.pack $ BSLU.toString bs
    Nothing ->
      if code >= 0 && code <= 0x7F
      then T.singleton (chr code)
      else "\xFFFD"

sjisletters :: PSParser T.Text
sjisletters = do
  txt <- (many1 $ choice [ try $ ')' <$ (string "\\)")
                         , try $ '(' <$ (string "\\(")
                         , try $ (chr 10) <$ (string "\\n")
                         , try $ (chr 13) <$ (string "\\r")
                         , try $ (chr 8) <$ (string "\\b")
                         , try $ (chr 9) <$ (string "\\t")
                         , try $ (chr 12) <$ (string "\\f")
                         , try $ (chr 92) <$ (string "\\\\")
                         , try $ chr <$> ((string "\\") *> octnum)
                         , try $ noneOf ")"
                         ])
  let codes = sjisBytesToCodes $ map ord txt
  return $ T.concat $ map sjisCodeToText codes

unicodeletters :: PSParser T.Text
unicodeletters = do
  txt <- (many1 $ choice [ try $ ')' <$ (string "\\)")
                         , try $ '(' <$ (string "\\(")
                         , try $ (chr 10) <$ (string "\\n")
                         , try $ (chr 13) <$ (string "\\r")
                         , try $ (chr 8) <$ (string "\\b")
                         , try $ (chr 9) <$ (string "\\t")
                         , try $ (chr 12) <$ (string "\\f")
                         , try $ (chr 92) <$ (string "\\\\")
                         , try $ chr <$> ((string "\\") *> octnum)
                         , try $ noneOf ")"
                         ])
  let codes = unicodeBytesToCodes $ map ord txt
  return $ T.concat $ map unicodeCodeToText codes

jisletters :: PSParser T.Text
jisletters = do
  txt <- (many1 $ choice [ try $ ')' <$ (string "\\)")
                         , try $ '(' <$ (string "\\(")
                         , try $ (chr 10) <$ (string "\\n")
                         , try $ (chr 13) <$ (string "\\r")
                         , try $ (chr 8) <$ (string "\\b")
                         , try $ (chr 9) <$ (string "\\t")
                         , try $ (chr 12) <$ (string "\\f")
                         , try $ (chr 92) <$ (string "\\\\")
                         , try $ chr <$> ((string "\\") *> octnum)
                         , try $ noneOf ")"
                         ])
  let codes = jisBytesToCodes $ map ord txt
  return $ T.concat $ map jisCodeToText codes

octletters :: PSParser T.Text
octletters = do
  char '('
  lets <- manyTill octletter (try $ char ')')
  spaces
  return $ T.concat lets

hexDecodeUTF16BE :: T.Text -> T.Text
hexDecodeUTF16BE s =
  let bytestring = B.pack ((map read . map ("0x"<>) . map T.unpack . T.chunksOf 2) s :: [Word8])
  in decodeUtf16BE bytestring

adobeOneSix :: Int -> T.Text
adobeOneSix a = case Map.lookup a adobeJapanOneSixMap of
  Just cs -> T.pack $ BSLU.toString cs
  Nothing -> T.pack $ "[" ++ show a ++ "]"

lookupUcs :: Maybe Encoding -> CMap -> Int -> PSParser T.Text
lookupUcs enc m h = case Map.lookup h m of
  Just ucs -> return ucs
  Nothing ->
    case enc of
      Just SJISmap -> return $ sjisCodeToText h
      Just UnicodeMap -> return $ unicodeCodeToText h
      Just JISmap -> return $ jisCodeToText h
      _ | Map.null m ->
          case Map.lookup h adobeJapanOneSixMap of
            Just cs -> return $ T.pack $ BSLU.toString cs
            Nothing -> do
              updateState (\s -> s {warnings = UnmappedCid h : warnings s})
              return $ adobeOneSix h
      _ -> return $ T.singleton (chr h)

cidletters = choice [try hexletter, try octletter]

hexletter :: PSParser T.Text
hexletter = do
  st <- getState
  let cmap = Map.findWithDefault Map.empty (curfont st) (cmaps st)
  -- Single code inside a hex string fragment (legacy path; prefer hexletters).
  hexDigits <- choice [ try $ count 2 $ oneOf "0123456789ABCDEFabcdef"
                      , try $ (:"0") <$> (oneOf "0123456789ABCDEFabcdef")
                      ]
  case readHex hexDigits of
    [(h, "")] -> lookupUcs (Map.lookup (curfont st) (fontmaps st)) cmap h
    _ -> return "????"

octletter :: PSParser T.Text
octletter = do
  st <- getState
  let cmap = Map.findWithDefault Map.empty (curfont st) (cmaps st)
  o <- octnum
  lookupUcs (Map.lookup (curfont st) (fontmaps st)) cmap o

psletter :: Map.Map Char T.Text -> PSParser T.Text
psletter fontmap = do
  c <- try (char '\\' >> oneOf "\\()")
       <|>
       try (octToChar . readOct <$> (char '\\' >> (count 3 $ oneOf "01234567")))
       <|>
       noneOf "\\"
  return $ replaceWithDiff fontmap c
    where replaceWithDiff m c' = case Map.lookup c' m of
            Just s -> replaceWithCharDict s
            Nothing -> T.pack [c']
          replaceWithCharDict s = case Map.lookup (T.unpack s) pdfcharmap of
            Just cs -> cs
            Nothing -> if "/uni" `T.isPrefixOf` s
                       then readUni s
                       else s
          readUni s = case readHex (T.unpack $ T.drop 4 s) of
            [(i,"")] -> T.singleton $ chr i
            [(i,x)] -> T.pack (chr i : " ")
            _ -> s
          octToChar [(o,"")] = case Map.lookup o extendedAscii of
            Just c -> c
            Nothing -> chr o
          octToChar _ = '?'

cidletter :: T.Text -> PSParser T.Text
cidletter _ = do
  o1 <- octnum
  o2 <- octnum
  let d = 256 * o1 + o2
  lookupUcs Nothing Map.empty d

octnum :: PSParser Int
octnum = do
  d <- choice [ try $ escapedToDec <$> (char '\\' >> oneOf "nrtbf()\\")
              , try $ octToDec . readOct <$> (char '\\' >> (count 3 $ oneOf "01234567"))
              , try $ ord <$> noneOf "\\"
              ]
  return $ d
  where
    octToDec [(o, "")] = o
    octToDec _ = ord '?'
    escapedToDec 'n' = ord '\n'
    escapedToDec 'r' = ord '\r'
    escapedToDec 't' = ord '\t'
    escapedToDec 'b' = ord '\b'
    escapedToDec 'f' = ord '\f'
    escapedToDec '\\' = ord '\\'
    escapedToDec _ = 0

kern :: PSParser T.Text
kern = do
  t <- digitParam
  spaces
  return $ if t < -60.0 then " " else ""

pdfopTf :: PSParser T.Text
pdfopTf = do
  font <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  t <- digitParam
  spaces
  string "Tf"
  spaces
  st <- getState
  let ff = fontfactor st
  updateState (\s -> s{ curfont = T.pack font
                      , fontfactor = t
                      , linex = t
                      , liney = t})
  return ""

pdfopTD :: PSParser T.Text
pdfopTD = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "TD"
  spaces
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      (a,b,c,d,tmx,tmy) = text_m st
      needBreakByX = a*t1 + c*t2 + tmx < ax
      needBreakByY = abs (b*t1 + d*t2 + tmy - ay) > ff
      needBreak = (needBreakByX || needBreakByY) && not (text_break st)
  updateState (\s -> s { absolutex = if needBreak then 0 else a*t1 + c*t2 + tmx
                       , absolutey = b*t1 + d*t2 + tmy
                       , liney = -t2
                       , text_m = (a,b,c,d, a*t1 + c*t2 + tmx, b*t1 + d*t2 + tmy)
                       , text_break = needBreak
                       })
  return $ if needBreak 
           then (desideParagraphBreak t1 t2 lx ly lm ff)
           else if a*t1 + c*t2 + tmx > ax + 2*ff
                then " " else ""

pdfopTd :: PSParser T.Text
pdfopTd = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "Td"
  spaces
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      (a,b,c,d,tmx,tmy) = text_m st
      needBreakByX = a*t1 + c*t2 + tmx < ax
      needBreakByY = abs (b*t1 + d*t2 + tmy - ay) > ff
      needBreak = (needBreakByX || needBreakByY) && not (text_break st)
  updateState (\s -> s { absolutex = if needBreak then 0 else a*t1 + c*t2 + tmx
                       , absolutey = b*t1 + d*t2 + tmy
                       , linex = lx
                       , liney = ly
                       , text_m = (a,b,c,d, a*t1 + c*t2 + tmx, b*t1 + d*t2 + tmy)
                       , text_break = needBreak
                       })
  return $ if needBreak 
           then (desideParagraphBreak t1 t2 lx ly lm ff)
           else if a*t1 + c*t2 + tmx > ax + 2*ff
                then " " else ""

pdfopTw :: PSParser T.Text
pdfopTw = do
  tw <- digitParam
  spaces
  string "Tw"
  spaces
  st <- getState
  let ff = fontfactor st
  updateState (\s -> s { fontfactor = tw
                       })
  return $ ""

pdfopTL :: PSParser T.Text
pdfopTL = do
  tl <- digitParam
  spaces
  string "TL"
  spaces
  st <- getState
  let ff = fontfactor st
  updateState (\s -> s { liney = ff + tl
                       })
  return $ ""

pdfopTz :: PSParser T.Text
pdfopTz = do
  tz <- digitParam
  spaces
  string "Tz"
  spaces
  st <- getState
  let ff = fontfactor st
  updateState (\s -> s { linex = ff + tz
                       })
  return $ ""

pdfopTc :: PSParser T.Text
pdfopTc = do
  tc <- digitParam
  spaces
  string "Tc"
  spaces
  return $ ""

pdfopTr :: PSParser T.Text
pdfopTr = do
  tr <- digitParam
  spaces
  string "Tr"
  spaces
  st <- getState
  let ff = fontfactor st
  return $ ""

pdfopTs :: PSParser T.Text
pdfopTs = do
  tc <- digitParam
  spaces
  string "Ts"
  spaces
  return $ ""

desideParagraphBreak :: Double -> Double -> Double -> Double -> Double -> Double 
                     -> T.Text
desideParagraphBreak t1 t2 lx ly lm ff = T.pack $
  (if abs t2 > 1.8*ly || (lx - t1) < lm
   then " "
   else "")

pdfopTm :: PSParser T.Text
pdfopTm = do
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
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      (_,_,_,_,tmx,tmy) = text_m st
      newff = abs $ (a+d)/2
      needBreakByX = a*tmx + c*tmy + e < ax
      needBreakByY = abs (b*tmx + d*tmy + f - ay) > ff
      needBreak = (needBreakByX || needBreakByY) && not (text_break st)
      newst = st { absolutex = e
                 , absolutey = f
                 , linex = lx
                 , liney = ly
                 , text_lm = (a,b,c,d,e,f)
                 , text_m = (a,b,c,d,e,f)
                 , text_break = needBreak
                 }
  putState newst
  return $ T.empty

pdfopcm :: PSParser T.Text
pdfopcm = do
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
  st <- getState
  -- What should be the effect on the page text?
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      (_,_,_,_,tmx,tmy) = text_m st
      needBreakByX = a*tmx + c*tmy + e < ax
      needBreakByY = abs (b*tmx + d*tmy + f - ay) > ff
      needBreak = (needBreakByX || needBreakByY) && not (text_break st)
      newst = st { absolutex = ax
                 , absolutey = ay
                 , linex = lx
                 , liney = ly
                 , text_lm = (a,b,c,d,e,f)
                 , text_m = (a,b,c,d,e,f)
                 , text_break = needBreak
                 }
  putState newst
  return T.empty

pdfopTast :: PSParser T.Text
pdfopTast = do
  string "T*"
  st <- getState
  let ax = absolutex st
      ay = absolutey st
      lx = linex st
      ly = liney st
      lm = leftmargin st
      ff = fontfactor st
      (a,b,c,d,tmx,tmy) = text_m st
      needBreakByX = tmx < ax
      needBreakByY = d*ly + tmy > ly
      needBreak = needBreakByX || needBreakByY
  updateState (\s -> s { absolutex = if needBreak then 0 else tmx
                       , absolutey = tmy + ly
                       , linex = lx
                       , liney = ly
                       , text_m = (a,b,c,d, c*ly + tmx, d*ly + tmy)
                       , text_break = needBreak
                       })
  return ""

digitParam :: PSParser Double
digitParam = do
  sign <- (char '-' >> return "-") <|> return ""
  num <- ((++) <$> (("0"++) <$> (string ".")) <*> many1 digit)
         <|>
         ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
  return $ parsePdfNumber $ sign ++ num

hexParam :: Parser T.Text
hexParam = do
  char '<'
  lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
  return $ T.pack lets
