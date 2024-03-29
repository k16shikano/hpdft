{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module PDF.ContentStream 
       ( parseStream
       , parseColorSpace
       ) where

import Data.Char (chr, ord)
import Data.String (fromString)
import Data.List (isPrefixOf, dropWhileEnd)
import Numeric (readOct, readHex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import GHC.Word (Word8)
import qualified Data.ByteString as B (pack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (ByteString, pack)
import qualified Data.ByteString.Char8 as BSSC (unpack)
import qualified Data.ByteString.Lazy.UTF8 as BSLU (toString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf16BE)

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.ByteString.Lazy
import Control.Applicative

import Debug.Trace

import PDF.Definition
import PDF.Object
import PDF.Character (pdfcharmap, extendedAscii, adobeJapanOneSixMap)

type PSParser a = GenParser Char PSR a

parseContentStream p st = runParser p st ""

parseStream :: PSR -> PDFStream -> PDFStream
parseStream psr pdfstream = 
  case parseContentStream (T.concat <$> (spaces >> many (try elems <|> skipOther))) psr pdfstream of
    Left  err -> error $ "Nothing to be parsed: " ++ (show err) 
    Right str -> BSLC.pack $ BSSC.unpack $ encodeUtf8 str

parseColorSpace :: PSR -> BSLC.ByteString -> [T.Text]
parseColorSpace psr pdfstream = 
  case parseContentStream (many (choice [ try colorSpace
                                        , try $ T.concat <$> xObject
                                        , (T.empty <$ elems)
                                        ])) psr pdfstream of
    Left  err -> error "Nothing to be parsed"
    Right str -> str


-- | Parsers for Content Stream

elems :: PSParser T.Text
elems = choice [ try pdfopBT 
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
               , try $ T.empty <$ xObject
               , try graphicState
               , try pdfopcm
               , try $ T.empty <$ colorSpace
               , try $ T.empty <$ renderingIntent
               , try pdfopBDC
               , try pdfopBMC
               , try pdfopEMC
               , unknowns
               ]

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
  updateState (\s -> s {colorspace = gs})
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
  return $ map T.pack xobjcs

pdfopBT :: PSParser T.Text
pdfopBT = do
  st <- getState
  updateState (\s -> s{text_m = (1,0,0,1,0,0), text_break = False})
  string "BT"
  spaces
  t <- manyTill elems (try $ string "ET")
  spaces
  return $ T.concat t

-- should have refined according to the section 10.5 of PDF reference

pdfopBMC :: PSParser T.Text
pdfopBMC = do
  tag <- (++) <$> string "/" <*> manyTill anyChar (try space)
  spaces
  string "BMC"
  spaces
  manyTill elems (try $ string "EMC")
  spaces
  return T.empty

pdfopBDC :: PSParser T.Text
pdfopBDC = do
  tag <- name
  prop <- propertyList
  spaces
  string "BDC"
  spaces
  case tag of
    "/Span" 
      | "/ActualText" == (fst prop)
        -> do {spaces >> manyTill elems (try $ string "EMC") >> return (snd prop)}
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
  -- linebreak within (...) is parsed as Tj
  return $ case runParser elems st "" $ BSLC.pack ((Data.List.dropWhileEnd (=='\\') ps)++")Tj") of
             Right xs -> xs
             Left e -> case runParser elems st "" $ BSLC.pack ("("++ps) of
               Right xs -> xs
               Left e -> case ps of
                 "" -> ""
                 otherwise -> T.pack $ "[[[UNKNOWN STREAM:" ++ take 100 (show ps) ++ "]]]"

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
  let cmap = fromMaybe [] (lookup (curfont st) (cmaps st))
      letterParser = case lookup (curfont st) (fontmaps st) of
        Just (Encoding m) -> psletter m
        Just (CIDmap s) -> cidletter s
        Just (WithCharSet s) -> try $ bytesletter cmap <|> cidletters
        Just NullMap -> psletter []
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
  return $ byteStringToText cmap txt
  where
    byteStringToText :: CMap -> String -> T.Text
    byteStringToText cmap str = T.concat $ map (toUcs cmap) $ asInt16 $ map ord str

    asInt16 :: [Int] -> [Int]
    asInt16 [] = []
    asInt16 (a:[]) = [a] --error $ "Can not read string "++(show a)
    asInt16 (a:b:rest) = (a * 256 + b):(asInt16 rest)

    -- for debug
    -- myToUcs cmap x = if x == 636 then trace (show cmap) $ toUcs cmap x else toUcs cmap x

hexletters :: PSParser T.Text
hexletters = do
  char '<'
  lets <- manyTill hexletter (try $ char '>')
  spaces
  return $ T.concat lets

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
  Nothing -> T.pack $ "[" ++ (show a) ++ "]"

toUcs :: CMap -> Int -> T.Text
toUcs m h = case lookup h m of
  Just ucs -> T.pack ucs
  Nothing -> if m == [] then adobeOneSix h else T.pack [chr h]

cidletters = choice [try hexletter, try octletter]

hexletter :: PSParser T.Text
hexletter = do
  st <- getState
  let font = curfont st
      cmap = fromMaybe [] (lookup font (cmaps st))
  (hexToString cmap . readHex) <$> choice [ try $ count 4 $ oneOf "0123456789ABCDEFabcdef"
                                          , try $ count 2 $ oneOf "0123456789ABCDEFabcdef"
                                          , try $ (:"0") <$> (oneOf "0123456789ABCDEFabcdef")
                                          ]
  where hexToString m [(h,"")] = toUcs m h
        hexToString _ _ = "????"

octletter :: PSParser T.Text
octletter = do
  st <- getState
  let cmap = fromMaybe [] (lookup (curfont st) (cmaps st))
  o <- octnum
  return $ toUcs cmap o

psletter :: [(Char,String)] -> PSParser T.Text
psletter fontmap = do
  c <- try (char '\\' >> oneOf "\\()")
       <|>
       try (octToChar . readOct <$> (char '\\' >> (count 3 $ oneOf "01234567")))
       <|>
       noneOf "\\"
  return $ replaceWithDiff fontmap c
    where replaceWithDiff m c' = case lookup c' m of
            Just s -> replaceWithCharDict s
            Nothing -> T.pack [c']
          replaceWithCharDict s = case Map.lookup s pdfcharmap of
            Just cs -> cs
            Nothing -> if "/uni" `isPrefixOf` s
                       then readUni s
                       else T.pack s
          readUni s = case readHex (drop 4 s) of
            [(i,"")] -> T.singleton $ chr i
            [(i,x)] -> T.pack (chr i : " ")
            _ -> T.pack s
          octToChar [(o,"")] = case Map.lookup o extendedAscii of
            Just c -> c
            Nothing -> chr o
          octToChar _ = '?'

cidletter :: String -> PSParser T.Text
cidletter cidmapName = do
  o1 <- octnum
  o2 <- octnum
  let d = 256 * o1 + o2
  return $
    if cidmapName == "Adobe-Japan1"
    then adobeOneSix d
    else error $ "Unknown cidmap" ++ cidmapName

octnum :: PSParser Int
octnum = do
  d <- choice [ try $ escapedToDec <$> (char '\\' >> oneOf "nrtbf()\\")
              , try $ octToDec . readOct <$> (char '\\' >> (count 3 $ oneOf "01234567"))
              , try $ ord <$> noneOf "\\"
              ]
  return $ d
  where
    octToDec [(o, "")] = o
    octToDec _ = error "Unable to take Character in Octet"
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
  updateState (\s -> s{ curfont = font
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
  sign <- many $ char '-'
  num <- ((++) <$> (("0"++) <$> (string ".")) <*> many1 digit)
         <|>
         ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
  return $ read $ sign ++ num

hexParam :: Parser T.Text
hexParam = do
  char '<'
  lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
  return $ T.pack lets
