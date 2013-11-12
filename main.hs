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
  BSL.putStrLn $ linearize root objs

getObjs :: BS.ByteString -> [PDFObj]
getObjs contents = case parse (many1 obj) "" contents of
  Left  err -> []
  Right rlt -> rlt

obj :: Parser PDFObj
obj = do
  many $ comment <|> char (chr 13)
  objn <- many1 digit <* string " 0 obj"
  object <- manyTill anyChar (try $ string "endobj")
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
  Just (PdfName "/Contents", ObjRef x) -> case findObjByRef x objs of
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

parsePDFObj :: PDFObj -> (Int,[Obj])
parsePDFObj (n,pdfobject) = case parse (spaces >> many1 (pdfobj <|> objother)) "" pdfobject of
  Left  err -> (n,[PdfNull])
  Right obj -> (n,obj)

comment :: Parser Char
comment = do
  char '%'
  manyTill anyChar $ char (chr 13)
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
pdfletters = PdfText <$> letters

pdfstream :: Parser Obj
pdfstream = PdfStream <$> stream

pdfnumber :: Parser Obj
pdfnumber = PdfNumber <$> digitParam

pdfhex :: Parser Obj
pdfhex = PdfHex <$> hexParam

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

parseDeflated :: BS.ByteString -> PDFStream
parseDeflated pdfstrem = case parse (concat <$> many (elems <|> skipOther)) "" pdfstrem of
  Left  err -> ""
  Right str -> BSL.pack str

elems :: Parser String
elems = choice [ try pdfopTf
               , try pdfopTD
               , try pdfopTm
               , try letters
               , array
               ]

skipOther :: Parser String
skipOther = do
  manyTill anyChar (try $ string "\n")
  return ""

array :: Parser String
array = do
  char '['
  str <- manyTill (letters <|> kern) (try $ char ']')
  return $ concat str

letters :: Parser String
letters = do
  char '('
  lets <- manyTill psLetter (try $ char ')')
  return $ lets

psLetter :: Parser Char
psLetter = do
  c <- (char '\\' >> oneOf "\\()") <|> noneOf "\\"
  return c
        
kern :: Parser String
kern = do
  t <- digitParam
  return $ if t < -100.0 then " " else ""

pdfopTf :: Parser String
pdfopTf = do
  font <- pdfname
  spaces
  t <- digitParam
  string "Tf"
  return ""

pdfopTD :: Parser String
pdfopTD = do
  t1 <- digitParam
  spaces
  t2 <- digitParam
  spaces
  string "TD"
  return $ desideParagraphBreak t1 t2

pdfopTm :: Parser String
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
  let t1 = e/a 
  let t2 = f/d
  return $ desideLineBreak t1 t2

desideParagraphBreak :: Double -> Double -> String
desideParagraphBreak t1 t2 = 
  (if t1 > 0.1
   then if t2 < -1.0 then "\n\n" else " "
   else "") 
  ++
  (if t2 < -2.0 then "\n\n" else " ")

desideLineBreak :: Double -> Double -> String
desideLineBreak t1 t2 = 
  if t2 > 40.0 then "\n\n" else ""

digitParam :: Parser Double
digitParam = do 
  sign <- many $ char '-'
  num <- ((++) <$> (("0"++) <$> string ".") <*> many1 digit)
         <|>
         ((++) <$> (many1 digit) <*> ((++) <$> (many $ char '.') <*> many digit))
  return $ read $ sign ++ num

hexParam :: Parser String
hexParam = do
  char '<'
  lets <- manyTill (oneOf "0123456789abcdefABCDEF") (try $ char '>')
  return $ lets
