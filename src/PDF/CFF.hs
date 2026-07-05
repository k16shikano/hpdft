{-# LANGUAGE OverloadedStrings #-}

module PDF.CFF (encoding) where

import Numeric (readInt)
import Data.Char (chr, intToDigit)
import Data.List (isPrefixOf)

import Data.Word
import Data.Bits

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

import Data.Attoparsec.ByteString (Parser, parseOnly, word8, string)
import qualified Data.Attoparsec.ByteString as AP
import Data.Attoparsec.Combinator

import Control.Applicative

import qualified Data.Map as Map
import qualified Data.Text as T

import PDF.Definition

type SID = Integer

test f = do
  c <- BS.readFile f
  return $ encoding c

encoding :: ByteString -> Encoding
encoding c =
  case parseOnly (header >> index *> index) c of
    Left _ -> NullMap
    Right ds | null ds -> NullMap
    Right ds ->
      let encodings  = concatMap (parseEncoding c) ds
          charset    = concatMap (parseCharset c) ds
          strings    = case parseOnly stringInd c of
                         Right arr -> arr
                         Left _    -> []
      in Encoding $ Map.fromListWith (flip const) $
           map (findEncodings strings) $ zip charset encodings

  where
    findEncodings strings (char,enc) =
      case char of
        s | s > 390 ->
          let idx = fromInteger (char - 390 - 1)
          in (enc, stringToText $ safeAt strings idx)
          | s > 95 -> (enc, sidToText s)
          | otherwise -> (enc, T.singleton enc)

    safeAt xs i = if i >= 0 && i < length xs then xs !! i else ""

    -- defined in String INDEX of each font
    stringToText "a113" = "‡"
    stringToText "a114" = "・"
    stringToText "trianglesolid" = "▲"
    stringToText x = "[CFF:String " <> T.pack x <> "]"

    -- pre-defined in Appendix C of CFF specs
    sidToText n =
      let i = fromInteger n
          chars = complementSID 0 predefinedChars
      in if i >= 0 && i < length chars then T.singleton (chars !! i) else "?"

parseTopDictInd :: ByteString -> [ByteString]
parseTopDictInd c = case parseOnly (header >> index *> index) c of
  Right ds -> ds
  Left _   -> []

parseEncoding :: ByteString -> ByteString -> [Char]
parseEncoding c d = case parseOnly (many1 dict) d of
  Right dictData -> case lookup [16] dictData of 
    Just (DictInt 0:[]) -> []
    Just (DictInt 1:[]) -> []
    Just (DictInt n:[]) ->
      case parseOnly encodingArray $ BS.drop (fromIntegral n) c of
        Right arr -> map (chr . fromInteger) arr
        Left _    -> []
    Just _    -> []
    Nothing   -> []
  Left _ -> []

parseFontname c d = case parseOnly (many1 dict) d of
  Right dictData -> case lookup [2] dictData of
    Just (DictInt n:[]) ->
      case parseOnly stringInd c of
        Right arr ->
          let idx = fromIntegral n - 390 - 1
          in if idx >= 0 && idx < length arr then [arr !! idx] else []
        Left _ -> []
    Just _    -> []
    Nothing   -> []
  Left _ -> []

parseCharset c d = case parseOnly (many1 dict) d of
  Right dictData -> case lookup [15] dictData of
    Just (DictInt offset:[]) -> 
      case parseOnly (charsetData $ charStringsInd c dictData) $
           BS.drop (fromIntegral offset) c of
        Right arr -> arr
        Left _    -> []
    Just _    -> []
    Nothing   -> []
  Left _ -> []

charStringsInd c dictData = 
  case lookup [17] dictData of
    Just (DictInt offset:[]) -> 
      case parseOnly index (BS.drop (fromIntegral offset) c) of
        Right ind -> ind
        Left _    -> []
    Nothing -> []

nameInd = map BSC.unpack <$> (header *> index)

dictInd = BSC.concat <$> (header >> index *> index)

stringInd = map BSC.unpack <$> (header >> index >> index *> index)

charsetData :: [ByteString] -> Parser [Integer]
charsetData ind = do
  format <- getCard 1
  charsetObj format
  
  where
    -- .notdef must be excluded, so minus one
    charsetObj 0 = count (length ind - 1) getSID 


encodingArray :: Parser [Integer]
encodingArray = do
  format <- getCard 1
  p <- fromInteger <$> getCard 1
  encodeObj format p

  where
    encodeObj :: Integer -> Int -> Parser [Integer]
    encodeObj 0 p = count (p - 1) (getCard 1)
    encodeObj 1 p = concat <$> count p getRange1
    encodeObj _ _ = return []

    getRange1 :: Parser [Integer]
    getRange1 = do
      first <- getCard 1
      nleft <- getCard 1
      return $ [first .. first + nleft]

data DictOp = DictInt Int | DictReal Double
  deriving (Show)

dict = (flip (,)) <$> (manyTill dictOp (try $ lookAhead dictKey)) <*> dictKey

dictOp :: Parser DictOp
dictOp = do
  b0 <- fromInteger <$> getCard 1
  opEnc b0
  
  where
    opEnc :: Int -> Parser DictOp
    opEnc b0 | b0 >= 32 && b0 <= 246 = return $ DictInt $ b0 - 139
             | b0 >= 247 && b0 <= 250 = do
                 b1 <- fromInteger <$> getCard 1
                 return $ DictInt $ (b0 - 247) * 256 + b1 + 108
             | b0 >= 251 && b0 <= 254 = do
                 b1 <- fromInteger <$> getCard 1
                 return $ DictInt $ - (b0 - 251) * 256 - b1 - 108
             | b0 == 28 = do
                 b1 <- fromInteger <$> getCard 1
                 b2 <- fromInteger <$> getCard 1
                 return $ DictInt $ b1 `shiftL` 8 .|. b2
             | b0 == 29 = do
                 b1 <- fromInteger <$> getCard 1
                 b2 <- fromInteger <$> getCard 1
                 b3 <- fromInteger <$> getCard 1
                 b4 <- fromInteger <$> getCard 1
                 return $ DictInt $  b1 `shiftL` 24 .|. b2 `shiftL` 16 .|. b3 `shiftL` 8 .|. b4
             | b0 == 30 = do
                 r <- many1 $ AP.satisfy (\w -> (240 .|. w) `xor` 255 /= 0)
                 f <- getCard 1
                 return $ DictReal $ readNibble r f
             | otherwise = fail (show b0)
    readNibble s1 s2 = 0

dictKey :: Parser [Word8]
dictKey = do
  key <- AP.choice [ try $ many1 $ AP.satisfy $ AP.inClass "\0-\5\13-\18"
                   , try $ (flip (flip (:) . (:[])))
                     <$> AP.satisfy (==12) <*> (AP.satisfy $ AP.inClass "\0-\8\20-\23\30-\38")
                   ]
  return key

index :: Parser [ByteString]
index = do
  indexCount <- fromInteger <$> getCard 2
  offSize <- fromInteger <$> getCard 1
  offsets <- map fromInteger <$> count (indexCount+1) (getCard offSize)
  indexData <- repeatFor offsets
  return indexData
  where
    repeatFor ls = sequence $ map AP.take $ differenciate ls
    differenciate ls = tail $ zipWith subtract (0:ls) ls

header :: Parser Integer
header = do
  major <- getCard 1
  minor <- getCard 1
  hrdSize <- getCard 1
  offSize <- getCard 1
  return $ major

getCard :: Int -> Parser Integer
getCard n = fromBytes <$> AP.take n

getSID :: Parser SID
getSID = fromBytes <$> AP.take 2

fromBytes :: ByteString -> Integer
fromBytes = BS.foldl' f 0
  where
    f a b = a `shiftL` 8 .|. fromIntegral b

complementSID _ [] = []
complementSID i arr@((n,c):rest) 
  | i == n = c:(complementSID (i+1) rest)
  | otherwise = ' ':(complementSID (i+1) arr)

predefinedChars = 
  [ (1, ' ')
  , (2, '!')
  , (3, '"')
  , (4, '#')
  , (5, '$')
  , (6, '%')
  , (7, '&')
  , (8, '’')
  , (9, '(')
  , (10, ')')
  , (11, '*')
  , (12, '+')
  , (13, ',')
  , (14, '-')
  , (15, '.')
  , (16, '/')
  , (17, '0')
  , (18, '1')
  , (19, '2')
  , (20, '3')
  , (21, '4')
  , (22, '5')
  , (23, '6')
  , (24, '7')
  , (25, '8')
  , (26, '9')
  , (27, ':')
  , (28, ';')
  , (29, '<')
  , (30, '=')
  , (31, '>')
  , (32, '?')
  , (33, '@')
  , (34, 'A')
  , (35, 'B')
  , (36, 'C')
  , (37, 'D')
  , (38, 'E')
  , (39, 'F')
  , (40, 'G')
  , (41, 'H')
  , (42, 'I')
  , (43, 'J')
  , (44, 'K')
  , (45, 'L')
  , (46, 'M')
  , (47, 'N')
  , (48, 'O')
  , (49, 'P')
  , (50, 'Q')
  , (51, 'R')
  , (52, 'S')
  , (53, 'T')
  , (54, 'U')
  , (55, 'V')
  , (56, 'W')
  , (57, 'X')
  , (58, 'Y')
  , (59, 'Z')
  , (60, '{')
  , (61, '/')
  , (62, '}')
  , (63, '^')
  , (64, '_')
  , (65, '‘')
  , (66, 'a')
  , (67, 'b')
  , (68, 'c')
  , (69, 'd')
  , (70, 'e')
  , (71, 'f')
  , (72, 'g')
  , (73, 'h')
  , (74, 'i')
  , (75, 'j')
  , (76, 'k')
  , (77, 'l')
  , (78, 'm')
  , (79, 'n')
  , (80, 'o')
  , (81, 'p')
  , (82, 'q')
  , (83, 'r')
  , (84, 's')
  , (85, 't')
  , (86, 'u')
  , (87, 'v')
  , (88, 'w')
  , (89, 'x')
  , (90, 'y')
  , (91, 'z')
  , (92, '[')
  , (93, 'ˉ')
  , (94, ']')
  , (95, '~')
  , (96, '¡')
  , (97, '¢')
  , (98, '£')
  , (99, '/')
  , (100, '¥')
  , (101, 'ƒ')
  , (102, '§')
  , (103, '$')
  , (104, '\'')
  , (105, '“')
  , (106, '«')
  , (107, '‹')
  , (108, '›')
  , (109, 'ﬁ')
  , (110, 'ﬂ')
  , (111, '–')
  , (112, '†')
  , (113, '‡')
  , (114, '·')
  , (115, '❡')
  , (116, '・')
  , (117, '‚')
  , (118, '„')
  , (119, '”')
  , (120, '»')
  , (121, '…')
  , (122, '‰')
  , (123, '¿')
  , (124, '`')
  , (125, '´')
  , (126, '^')
  , (127, '~')
  , (128, '¯')
  , (129, '˘')
  , (130, '˙')
  , (131, '¨')
  , (132, '°')
  , (133, '¸')
  , (134, '˝')
  , (135, '˛')
  , (136, 'ˇ')
  , (137, '—')
  , (138, 'Æ')
  , (139, 'ª')
  , (140, 'Ł')
  , (141, 'Ø')
  , (142, 'Œ')
  , (143, 'º')
  , (144, 'æ')
  , (145, 'ı')
  , (146, 'ł')
  , (147, 'ø')
  , (148, 'œ')
  , (149, 'ẞ')
  , (150, '¹')
  , (151, '￢')
  , (152, 'µ')
  , (153, '™')
  , (154, 'Ð')
  , (155, '½')
  , (156, '±')
  , (157, 'Þ')
  , (158, '¼')
  , (159, '÷')
  , (160, '¦')
  , (161, '°')
  , (162, 'þ')
  , (163, '¾')
  , (164, '²')
  , (165, '®')
  , (166, '－')
  , (167, 'ð')
  , (168, '×')
  , (169, '³')
  , (170, 'Ⓒ')
  , (171, 'Á')
  , (172, 'Â')
  , (173, 'Ä')
  , (174, 'À')
  , (175, 'Å')
  , (176, 'Ã')
  , (177, 'Ç')
  , (178, 'É')
  , (179, 'Ê')
  , (180, 'Ë')
  , (181, 'È')
  , (182, 'Í')
  , (183, 'Î')
  , (184, 'Ï')
  , (185, 'Ì')
  , (186, 'Ñ')
  , (187, 'Ó')
  , (188, 'Ô')
  , (189, 'Ö')
  , (190, 'Ò')
  , (191, 'Õ')
  , (192, 'Š')
  , (193, 'Ú')
  , (194, 'Û')
  , (195, 'Ü')
  , (196, 'Ù')
  , (197, 'Ý')
  , (198, 'Ÿ')
  , (199, 'Ž')
  , (200, 'á')
  , (201, 'â')
  , (202, 'ä')
  , (203, 'à')
  , (204, 'å')
  , (205, 'ã')
  , (206, 'ç')
  , (207, 'é')
  , (208, 'ê')
  , (209, 'ë')
  , (210, 'è')
  , (211, 'í')
  , (212, 'î')
  , (213, 'ï')
  , (214, 'ì')
  , (215, 'ñ')
  , (216, 'ó')
  , (217, 'ô')
  , (218, 'ö')
  , (219, 'ò')
  , (220, 'õ')
  , (221, 'š')
  , (222, 'ú')
  , (223, 'û')
  , (224, 'ü')
  , (225, 'ù')
  , (226, 'ý')
  , (227, 'ÿ')
  , (228, 'ž')
  ]
