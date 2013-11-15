{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import System.IO
import Data.Char (chr)
import Data.List (find)
import Numeric (readOct)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import Text.Parsec.ByteString
import Codec.Compression.Zlib (decompress)

import Debug.Trace

type PDFBS = (Int,BS.ByteString)
type PDFObj = (Int,[Obj])
type PDFStream = BSL.ByteString
data Obj = PdfDict Dict -- [(Obj, Obj)]
         | PdfText String 
         | PdfStream PDFStream
         | PdfNumber Double 
         | PdfHex String
         | PdfBool Bool
         | PdfArray [Obj]
         | PdfName String 
         | ObjRef Int
         | ObjOther String
         | PdfNull
         deriving (Show, Eq)
type Dict =  [(Obj,Obj)]
type FontMap = [(Char,String)]

type PSParser a = GenParser Char (Double,Double,Double,Double) a

--initstate = (0,0,70,660)
initstate = (0,0,0,700)
topPt = 700
bottomPt = 0
leftMargin = 0

-- First: grub objects
-- Second: parse within each object, deflating its stream
-- Third: linearize stream

main = do
  (fileName:_) <- getArgs
  contents <- BS.readFile fileName
  let objs = map parsePDFObj $ getObjs contents
  let root = case rootRef contents of
        Just r  -> r
        Nothing -> 0
--  putStrLn $ show $ grubFontDiff 1666 objs
--  putStrLn $ show $ parsePDFObj (getObjs contents !! 659)
--  BSL.putStrLn $ decompressStream $ (getObjs contents) !! 168
  BSL.putStrLn $ linearize root objs

takeObjByRefs :: [PDFObj] -> Int -> [Obj]
takeObjByRefs objs n = case find t objs of  
  Just (_,obj) -> obj
  Nothing -> []
  where t (m,obj) = m == n
  
getObjs :: BS.ByteString -> [PDFBS]
getObjs contents = case parse (many1 obj) "" contents of
  Left  err -> []
  Right rlt -> rlt

obj :: Parser PDFBS
obj = do
  many $ comment <|> char (chr 13)
  objn <- many1 digit <* string " 0 obj"
  object <- manyTill anyChar (try $ string "endobj")
  spaces
  return $ (read objn, BS.pack object)

-- linearize objects

linearize :: Int -> [PDFObj] -> PDFStream
linearize parent objs = 
  case findObjByRef parent objs of
    Just os -> case findDictByType "/Catalog" os of
      Just dict -> case pages dict of 
        Just pr -> linearize pr objs
        Nothing -> ""
      Nothing -> case findDictByType "/Pages" os of
        Just dict -> case pagesKids dict of
          Just kidsrefs -> BSL.concat $ map (\f -> f objs) (map linearize kidsrefs)
          Nothing -> ""
        Nothing -> case findDictByType "/Page" os of
          Just dict -> contentsStream dict objs
          Nothing -> ""
    Nothing -> ""

findObjByRef :: Int -> [PDFObj] -> Maybe [Obj]
findObjByRef x pdfobjs = case find (isRefObj (Just x)) pdfobjs of
  Just (_,objs) -> Just objs
  Nothing -> Nothing

isRefObj :: Maybe Int -> PDFObj -> Bool
isRefObj (Just x) (y, objs) = if x==y then True else False
isRefObj _ _ = False

getRefs :: ((Obj,Obj) -> Bool) -> Maybe Dict -> Maybe Int
getRefs pred (Just objs) = case find pred objs of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing
getRefs _ _ = Nothing

parseRefsArray :: [Obj] -> [Int]
parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
parseRefsArray [] = []

findDictByType :: String -> [Obj] -> Maybe Dict
findDictByType typename objs = case find isDict objs of
  Just (PdfDict d) -> if isType d then Just d else Nothing 
  Nothing          -> Nothing
  where isType dict = (PdfName "/Type",PdfName typename) `elem` dict

pages :: Dict -> Maybe Int
pages dict = case find isPagesRef dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing

pagesKids :: Dict -> Maybe [Int]
pagesKids dict = case find isKidsRefs dict of
  Just (_, PdfArray arr) -> Just (parseRefsArray arr)
  Nothing                -> Nothing

contentsStream :: Dict -> [PDFObj] -> PDFStream
contentsStream dict objs = case find content dict of
  Just (PdfName "/Contents", ObjRef x) -> case findObjByRef x objs of
    Just contobjs -> case find isStream contobjs of
      Just (PdfStream strm) -> strm
      Nothing               -> ""
  Nothing -> ""
  where
    content (PdfName "/Contents", ObjRef x) = True
    content _                               = False

resourcesFont :: Dict -> [PDFObj] -> [(String, FontMap)]
resourcesFont dict objs = case find resources dict of
  Just (PdfName "/Resources", ObjRef x) -> case walkGraph x "/Font" objs of
    Just (PdfDict d) -> fonts d objs
    otherwise -> []
  otherwise -> []
  where
    resources (PdfName "/Resources", ObjRef x) = True
    resources _                                = False

grubFontDiff :: Int -> [PDFObj] -> [(String, FontMap)]
grubFontDiff ref objs = case walkGraph ref "/Resources" objs of
  Just (ObjRef rref) -> case walkGraph rref "/Font" objs of
    Just (PdfDict d) -> fonts d objs
    otherwise -> []
  otherwise -> []

findDictWithName :: String -> [Obj] -> Maybe Dict
findDictWithName name objs = case find isDict objs of
  Just (PdfDict d) -> case find isName d of 
    Just (_, PdfDict d') -> Just d'
    otherwise            -> Nothing
  Nothing          -> Nothing
  where isName (PdfName n, PdfDict _) = if name == n then True else False
        isName _                      = False

fonts :: Dict -> [PDFObj] -> [(String, FontMap)]
fonts dict objs = map pairwise dict
  where 
    pairwise (PdfName n, ObjRef r) = (n, findFontMap r objs)
    pairwise x = ("",[])

walkGraph :: Int -> String -> [PDFObj] -> Maybe Obj
walkGraph ref name objs = case findObjByRef ref objs of 
  Just os -> case find isDict os of
    Just (PdfDict d) -> case find isName d of
      Just (_, o) -> trace (show o) Just o
      otherwise -> Nothing
    otherwise -> Nothing
  otherwise -> Nothing
  where isName (PdfName n, _) = if name == n then True else False
        isName _              = False

findFontMap :: Int -> [PDFObj] -> FontMap
findFontMap x objs = case walkGraph x "/Encoding" objs of
  Just (ObjRef ref) -> case trace (show x) walkGraph ref "/Differences" objs of
    Just (PdfArray arr) -> charMap arr
    otherwise -> []
  otherwise -> []

charMap :: [Obj] -> FontMap
charMap objs = fontmap objs 0
  where fontmap (PdfNumber x : PdfName n : xs) i = if i < truncate x then 
                                                     (chr $ truncate x, n) : (fontmap xs $ incr x)
                                                     else 
                                                     (chr $ i, n) : (fontmap xs $ i+1)
        fontmap (PdfName n : xs) i               = (chr i, n) : (fontmap xs $ i+1)
        fontmap [] i                             = []
        incr x = (truncate x) + 1

isStream :: Obj -> Bool
isStream (PdfStream s) = True
isStream _             = False

isDict :: Obj -> Bool
isDict (PdfDict d) = True
isDict _           = False

isEncoding :: (Obj,Obj) -> Bool
isEncoding (PdfName "/Encoding", ObjRef x) = True
isEncoding (_,_)                           = False

isDifferences :: (Obj,Obj) -> Bool
isDifferences (PdfName "/Differences", PdfArray x) = True
isDifferences (_,_)                                = False

parseTrailer :: BS.ByteString -> Maybe Dict
parseTrailer bs = case parse trailer "" bs of
  Left  err -> Nothing
  Right rlt -> case parse (spaces >> pdfdictionary) "" rlt of
    Left  err  -> Nothing
    Right (PdfDict dict) -> Just dict
    Right other -> Nothing

trailer :: Parser BS.ByteString
trailer = do
  manyTill anyChar (try $ string "trailer")
  t <- manyTill anyChar (try $ string "startxref")
  return $ BS.pack t

rootRef :: BS.ByteString -> Maybe Int
rootRef bs = getRefs isRootRef $ parseTrailer bs

pagesRef :: Maybe Dict -> Maybe Int
pagesRef objs = getRefs isPagesRef objs

pageRef :: Maybe Dict -> Maybe Int
pageRef objs = getRefs isPageRef objs

isRootRef :: (Obj,Obj) -> Bool
isRootRef (PdfName "/Root", ObjRef x) = True
isRootRef (_,_) = False

isPagesRef :: (Obj,Obj) -> Bool
isPagesRef (PdfName "/Pages", ObjRef x) = True
isPagesRef (_,_)                        = False

isPageRef :: (Obj,Obj) -> Bool
isPageRef (PdfName "/Page", ObjRef x) = True
isPageRef (_,_)                       = False

isKidsRefs :: (Obj,Obj) -> Bool
isKidsRefs (PdfName "/Kids", PdfArray x) = True
isKidsRefs (_,_)                         = False

    
-- parse raw pdf

decompressStream :: PDFBS -> PDFStream
decompressStream (n,pdfobject) = 
  case parse (BSL.pack <$> 
              (manyTill anyChar (try $ string "stream\n") 
               *> manyTill anyChar (try $ string "endstream"))) "" pdfobject of
    Left err -> ""
    Right bs -> decompress bs

parsePDFObj :: PDFBS -> PDFObj
parsePDFObj (n,pdfobject) = case parse (spaces >> many1 (pdfobj <|> objother)) "" pdfobject of
  Left  err -> (n,[PdfNull])
  Right obj -> (n,obj)

comment :: Parser Char
comment = do
  char '%'
  manyTill anyChar $ oneOf "\r\n"
  return ' '

stream :: Parser PDFStream
stream = do
  string "stream\n"
  stm <- BSL.pack <$> manyTill anyChar (try $ string "endstream")
  return $ deflate stm

deflate :: PDFStream -> PDFStream
deflate = parseDeflated . BS.pack . BSL.unpack . decompress

pdfdictionary :: Parser Obj
pdfdictionary = PdfDict <$> (string "<<" >> spaces *> manyTill dictEntry (try $ spaces >> string ">>"))

dictEntry :: Parser (Obj, Obj)
dictEntry = (,) <$> pdfname <*> pdfobj

pdfarray :: Parser Obj
pdfarray = PdfArray <$> (string "[" >> spaces *> manyTill pdfobj (try $ spaces >> string "]"))

pdfname :: Parser Obj
pdfname = PdfName <$> ((++) <$> string "/" <*> manyTill anyChar (try $ lookAhead $ oneOf "] \n/")) <* spaces

pdfletters :: Parser Obj
pdfletters = PdfText <$> (char '(' *> manyTill pdfletter (try $ char ')'))
  where pdfletter  =  (char '\\' >> oneOf "\\()") <|> noneOf "\\"

pdfstream :: Parser Obj
pdfstream = PdfStream <$> stream

pdfnumber :: Parser Obj
pdfnumber = PdfNumber <$> pdfdigit
  where pdfdigit = do 
          sign <- many $ char '-'
          num <- ((++) <$> (("0"++) <$> string ".") <*> many1 digit)
                 <|>
                 ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
          spaces        
          return $ read $ sign ++ num

pdfhex :: Parser Obj
pdfhex = PdfHex <$> hex  
  where hex = do
          char '<'
          lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
          return $ lets

pdfbool :: Parser Obj
pdfbool = PdfBool <$> (True <$ string "true"
                       <|> 
                       False <$ string "false")

pdfnull :: Parser Obj
pdfnull = PdfNull <$ string "null"

pdfobj :: Parser Obj
pdfobj = choice [ try rrefs <* spaces
                , try pdfname <* spaces, try pdfnumber <* spaces, try pdfhex <* spaces
                , try pdfbool <* spaces, try pdfnull <* spaces
                , try pdfarray <* spaces, try pdfdictionary <* spaces, try pdfstream <* spaces
                , pdfletters <* spaces] <* spaces

rrefs :: Parser Obj
rrefs = do  
  objnum <- many1 digit
  spaces
  string "0 R"
  spaces
  return $ ObjRef (read objnum)

objother :: Parser Obj
objother = ObjOther <$> (manyTill anyChar space)


-- parse deflated pdf

parsePage p st = runParser p st ""

parseDeflated :: BS.ByteString -> PDFStream
parseDeflated pdfstrem = case parsePage (concat <$> many (elems <|> skipOther)) initstate pdfstrem of
  Left  err -> ""
  Right str -> BSL.pack str

elems :: PSParser String
elems = choice [ try pdfopTf
               , try pdfopTD
               , try pdfopTd
               , try pdfopTm
               , try pdfopTast
               , try letters
               , try bore
               , array
               ]

bore :: PSParser String
bore = do
  string "TJ"
  spaces
  return ""

skipOther :: PSParser String
skipOther = do
  a <- manyTill anyChar (try $ oneOf "\n")
  return $ ""

array :: PSParser String
array = do
  char '['
  str <- (manyTill (letters <|> kern) (try $ char ']'))
  return $ concat str

letters :: PSParser String
letters = do
  char '('
  (lx,ly,ax,ay) <- getState
  lets <- manyTill psletter (try $ char ')')
  return $ lets
--  return $ if ay > topPt || ay < bottomPt then "" else lets

psletter :: PSParser Char
psletter = do
  c <- try (char '\\' >> oneOf "\\()")
       <|>
       try (toChar . readOct <$> (char '\\' >> (count 3 $ oneOf "01234567")))
       <|>
       noneOf "\\"
  return c
    where toChar [] = '?'
          toChar [(o,_)] = chr o

kern :: PSParser String
kern = do
  t <- digitParam
  return $ if t < -60.0 then " " else ""

psname :: PSParser String
psname = ((++) <$> string "/" <*> manyTill anyChar (try space))

pdfopTf :: PSParser String
pdfopTf = do
  font <- psname
  spaces
  t <- digitParam
  spaces
  string "Tf"
  spaces
  return ""

pdfopTD :: PSParser String
pdfopTD = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "TD"
  spaces
  (lx,ly,ax,ay) <- getState
  updateState (\(lx,ly,ax,ay) -> (lx,ly,ax+(lx*t1),ay+(ly*t2)))
  return $ desideParagraphBreak (ax+(t1*lx)) (t2*ly)

pdfopTd :: PSParser String
pdfopTd = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "Td"
  spaces
  (lx,ly,ax,ay) <- getState
  let needBreak = t2 > 0
  updateState (\(lx,ly,ax,ay) -> (lx,ly,ax+t1,ay+t2))
  return $ if needBreak then "\n\n" else desideParagraphBreak t1 t2

desideParagraphBreak :: Double -> Double -> String
desideParagraphBreak t1 t2 = 
  (if abs (t1 - leftMargin) < 1
   then " "
   else if t2 < -15 then "\n\n" else " ")
  ++
  (if t2 < -15 then "\n\n" else " ")

pdfopTm :: PSParser String
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
  (lx,ly,ax,ay) <- getState
  updateState (\(lx,ly,ax,ay) -> (a,d,e,f))
  return $ if abs (f - ay) < 2*ly then 
             if e/lx < 1.5*lx then "" else " "
           else "\n\n"

pdfopTast :: PSParser String
pdfopTast = do
  string "T*"
  (lx,ly,ax,ay) <- getState
  updateState (\(lx,ly,ax,ay) -> (lx,ly,70,ay-ly))
  return " "

digitParam :: PSParser Double
digitParam = do 
  sign <- many $ char '-'
  num <- ((++) <$> (("0"++) <$> (string ".")) <*> many1 digit)
         <|>
         ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
  return $ read $ sign ++ num

hexParam :: Parser String
hexParam = do
  char '<'
  lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
  return $ lets
