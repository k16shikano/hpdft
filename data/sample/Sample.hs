{-# LANGUAGE OverloadedStrings #-}

import PDF.Definition
import PDF.Object
import PDF.PDFIO
import PDF.Outlines

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Debug.Trace

-- initstate = (0,0,70,660)
initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=700
                , leftmargin=0.0
                , top=700.0
                , bottom=0.0
                , fontfactor=1.0
                , curfont=""
                , fontmaps=[]
                , cmaps=[]}


-----------------------------------------------
-- Show each PDF Object by its reference number
-----------------------------------------------

objectByRef filename ref = getObjectByRef ref (getPDFObjFromFile filename)

streamByRef filename ref = do
  obj <- getObjectByRef ref (getPDFObjFromFile filename) 
  return $ rawStream obj

contentByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- objectByRef filename ref
  BSL.putStrLn $ contentOfObject obj objs
  where contentOfObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> contentsStream dict initstate objs
            Nothing -> ""

rawContentByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- objectByRef filename ref
  BSL.putStrLn $ rawContentOfObject obj objs
  where rawContentOfObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> rawContentsStream dict objs
            Nothing -> ""
  
showPage filename page = do 
  pagetree <- refByPage filename
  contentByRef filename $ pagetree !! (page - 1)

showRawPage filename page = do
  pagetree <- refByPage filename
  rawContentByRef filename $ pagetree !! (page - 1)

cmapStreamByRef filename ref = do
  objs <- getPDFObjFromFile filename
  return $ toUnicode ref objs


---------------------------------------
-- Sort Object References in Page order
---------------------------------------

data  PageTree = Nop | Page Int | Pages [PageTree]
                 deriving Show

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


--------------------------
-- Get Whole Text from PDF
--------------------------
-- First: grub objects
-- Second: parse within each object, deflating its stream
-- Third: linearize stream

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


-------------------
-- Meta Information    
-------------------

showTitle filename = do
  d <- getInfo filename
  let title = 
        case findObjThroughDict d "/Title" of
          Just (PdfText s) -> s
          Just x -> show x
          Nothing -> error "No title anyway"
  putStrLn title
  return ()

showInfo filename = do
  d <- getInfo filename
  putStrLn $ toString 0 (PdfDict d)
  return ()

showOutlines filename = do
  d <- getOutlines filename
  putStrLn $ show d
  return ()

