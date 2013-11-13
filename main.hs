{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.Directory
import System.IO
import Data.Char
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Text.Parsec hiding (many, (<|>))
import Control.Applicative
import Text.Parsec.ByteString
import Codec.Compression.Zlib (decompress)

import Debug.Trace

type PDFObj = (Int,BS.ByteString)
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

type PSParser a = GenParser Char (Double,Double,Double,Double) a

--initstate = (0,0,70,660)
initstate = (0,0,0,0)
topPt = 1000
bottomPt = 0

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
  let ob = getObjs contents
  BSL.putStrLn $ linearize root objs
--  putStrLn $ show $ parsePDFObj (getObjs contents !! 659)
--  BSL.putStrLn $ decompressStream $ (getObjs contents) !! 168
  
getObjs :: BS.ByteString -> [PDFObj]
getObjs contents = case parse (many1 obj) "" contents of
  Left  err -> []
  Right rlt -> rlt

obj :: Parser PDFObj
obj = do
  many $ comment <|> char (chr 13) 
  objn <- many1 digit <* string " 0 obj"
  object <- manyTill anyChar (try $ string "endobj")
  spaces
  return $ (read objn, BS.pack object)

-- linearize objects

linearize :: Int -> [(Int,[Obj])] -> PDFStream
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

findObjByRef :: Int -> [(Int,[Obj])] -> Maybe [Obj]
findObjByRef x pdfobjs = case find (isRefObj (Just x)) pdfobjs of
  Just (_,objs) -> Just objs
  Nothing -> Nothing

isRefObj :: Maybe Int -> (Int,[Obj]) -> Bool
isRefObj (Just x) (y, objs) = if x==y then True else False
isRefObj _ _ = False

getRefs :: ((Obj,Obj) -> Bool) -> Maybe Dict -> Maybe Int
getRefs pred (Just objs) = case find pred objs of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing

parseRefsArray :: [Obj] -> [Int]
parseRefsArray (ObjRef x:y) = (x:parseRefsArray y)
parseRefsArray [] = []

findDictByType :: String -> [Obj] -> Maybe Dict
findDictByType typename objs = case find isDict objs of
  Just (PdfDict d) -> if isType d then Just d else Nothing 
  Nothing          -> Nothing
  where isType dict = (PdfName "/Type",PdfName typename) `elem` dict
        isDict (PdfDict d) = True
        isDict _           = False

pages :: Dict -> Maybe Int
pages dict = case find isPagesRef dict of
  Just (_, ObjRef x) -> Just x
  Nothing            -> Nothing

pagesKids :: Dict -> Maybe [Int]
pagesKids dict = case find isKidsRefs dict of
  Just (_, PdfArray arr) -> Just (parseRefsArray arr)
  Nothing                -> Nothing

contentsStream :: Dict -> [(Int,[Obj])] -> PDFStream
contentsStream dict objs = case find content dict of
  Just (PdfName "/Contents", ObjRef x) -> case trace (show x) (findObjByRef x objs) of
    Just contobjs -> case find isStream contobjs of
      Just (PdfStream strm) -> strm
      Nothing               -> ""
  Nothing -> ""
  where
    content (PdfName "/Contents", ObjRef x) = True
    content _                               = False

isStream :: Obj -> Bool
isStream (PdfStream s) = True
isStream _             = False

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

decompressStream :: PDFObj -> PDFStream
decompressStream (n,pdfobject) = 
  case parse (BSL.pack <$> 
              (manyTill anyChar (try $ string "stream\n") 
               *> manyTill anyChar (try $ string "endstream"))) "" pdfobject of
    Left err -> ""
    Right bs -> decompress bs

parsePDFObj :: PDFObj -> (Int,[Obj])
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
pdfname = PdfName <$> ((++) <$> string "/" <*> manyTill anyChar (try space))

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
pdfobj = choice [ try rrefs
                , try pdfname, try pdfnumber, try pdfhex
                , try pdfbool, try pdfnull
                , try pdfarray, try pdfdictionary, try pdfstream
                , pdfletters] <* spaces

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
  return ""

skipOther :: PSParser String
skipOther = do
  manyTill anyChar (try $ oneOf "\n")
  return ""

array :: PSParser String
array = do
  char '['
  str <- manyTill (letters <|> kern) (try $ char ']')
  return $ concat str

letters :: PSParser String
letters = do
  char '('
  (lx,ly,ax,ay) <- getState
  lets <- manyTill psLetter (try $ char ')')
  return $ lets
--  return $ if ay > topPt || ay < bottomPt then "" else lets

psLetter :: PSParser Char
psLetter = do
  c <- (char '\\' >> oneOf "\\()") <|> noneOf "\\"
  return c
        
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
  (lx,ly,ax,ay) <- getState
  updateState (\(lx,ly,ax,ay) -> (1,1,ax+(lx*t1),ay+(ly*t2)))
  return $ desideParagraphBreak (ax+(t1*lx)) (t2*ly)

desideParagraphBreak :: Double -> Double -> String
desideParagraphBreak t1 t2 = 
  (if abs (t1 - 70) < 1
   then " "
   else if t2 < -12 then "\n\n" else " ")
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
