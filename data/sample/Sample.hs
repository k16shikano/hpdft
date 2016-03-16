{-# LANGUAGE OverloadedStrings #-}

import PDF.Definition
import PDF.Object
import PDF.PDFIO
import PDF.Outlines

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (nub)

import Debug.Trace

initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=0
                , leftmargin=0.0
                , top=0.0
                , bottom=0.0
                , fontfactor=1
                , curfont=""
                , fontmaps=[]
                , cmaps=[]
                , colorspace=""
                , xcolorspaces=[]
                }


-- | Show bare data in 'ref'ed-object.

objectByRef filename ref = getObjectByRef ref (getPDFObjFromFile filename)

-- | Show contents of page 'page' in 'filename'.

showPage filename page = do 
  pagetree <- refByPage filename
  contentByRef filename $ pagetree !! (page - 1)
  
-- | Show /Content referenced from the 'ref'ed-object in 'filename'.

contentByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- objectByRef filename ref
  BSL.putStrLn $ contentOfObject obj objs
  where contentOfObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> contentsStream dict initstate objs
            Nothing -> ""

-- | Show raw bytestring stream (without deflated) of page 'page' in 'filename'.

showRawPage filename page = do
  pagetree <- refByPage filename
  rawContentByRef filename $ pagetree !! (page - 1)

-- | Show raw bytestring stream (without deflated) of 'ref'ed-object.

streamByRef filename ref = do
  obj <- getObjectByRef ref (getPDFObjFromFile filename) 
  return $ rawStream obj

-- | Show raw bytestring stream (without deflated) in a /Content referenced from the 'ref'ed-object in 'filename'.

rawContentByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- objectByRef filename ref
  BSL.putStrLn $ rawContentOfObject obj objs
  where rawContentOfObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> rawContentsStream dict objs
            Nothing -> ""

-- | Show CMap information in object 'ref' in 'filename'.

cmapStreamByRef filename ref = do
  objs <- getPDFObjFromFile filename
  return $ toUnicode ref objs




data  PageTree = Nop | Page Int | Pages [PageTree]
                 deriving Show

-- | Sort object references in page order.

refByPage filename = do
  root <- getRootRef filename
  objs <- getPDFObjFromFile filename
  return $  pageTreeToList $ pageorder root objs

pageorder :: Int -> [PDFObj] -> PageTree
pageorder parent objs = 
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case pages dict of 
        Just pr -> pageorder pr objs
        Nothing -> Nop
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case pagesKids dict of
          Just kidsrefs -> Pages $ map (\f -> f objs) (map pageorder kidsrefs)
          Nothing -> Nop
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> Page parent
          Nothing -> Nop
    Nothing -> Nop

pageTreeToList :: PageTree -> [Int]
pageTreeToList (Pages ps) = concatMap pageTreeToList ps
pageTreeToList (Page n) = [n]
pageTreeToList Nop = []


-- | Get a whole text from 'filename'. It works as:
--    (1) grub objects
--    (2) parse within each object, deflating its stream
--    (3) linearize stream

pdfToText filename = do
  contents <- BS.readFile filename
  let objs = map parsePDFObj $ getObjs contents
  let rootref = case rootRef contents of
                  Just r  -> r
                  Nothing -> 0
  putStrLn $ show $ linearize rootref objs

linearize :: Int -> [PDFObj] -> PDFStream
linearize parent objs = 
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case pages dict of 
        Just pr -> linearize pr objs
        Nothing -> ""
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case pagesKids dict of
          Just kidsrefs -> BSL.concat $ map (\f -> f objs) (map linearize kidsrefs)
          Nothing -> ""
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> contentsStream dict initstate objs
          Nothing -> ""
    Nothing -> ""


-- | Show /Title from meta information in 'filename'

showTitle filename = do
  d <- getInfo filename
  let title = 
        case findObjThroughDict d "/Title" of
          Just (PdfText s) -> s
          Just x -> show x
          Nothing -> error "No title anyway"
  putStrLn title
  return ()

-- | Show /Info from meta information in 'filename'

showInfo filename = do
  d <- getInfo filename
  putStrLn $ toString 0 (PdfDict d)
  return ()

-- | Show /Outlines from meta information in 'filename'

showOutlines filename = do
  d <- getOutlines filename
  putStrLn $ show d
  return ()


-- | Show device color spaces of each page.

contentColorSpaceByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- objectByRef filename ref
  putStrLn $ show $ filter (/="") $ nub $ csOfObject obj objs
  where csOfObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> contentsColorSpace dict initstate objs
            Nothing -> error "Seems to be no color space in content stream"

-- | Show device color spaces of all pages.

showColorSpaces filename = do
  pages <- refByPage filename
  pagescs <- mapM (contentColorSpaceByRef filename) pages
  mapM (putStrLn . show) $ zip [1..] pagescs
  return ()