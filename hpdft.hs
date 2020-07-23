{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Definition

import PDF.Object
import PDF.DocumentStructure
import PDF.PDFIO
import PDF.Outlines

import System.Environment (getArgs)

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (nub)
import Data.Maybe (fromMaybe)

import Options.Applicative
import Data.Semigroup ((<>))

import Debug.Trace

initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=0
                , leftmargin=0.0
                , text_lm=(1,0,0,1,0,0)
                , text_m=(1,0,0,1,0,0)
                , text_break=False
                , top=0.0
                , bottom=0.0
                , fontfactor=1
                , curfont=""
                , fontmaps=[]
                , cmaps=[]
                , colorspace=""
                , xcolorspaces=[]
                }


main :: IO ()
main = hpdft =<< execParser opts
  where
    opts = info (options <**> helper)
      (fullDesc
       <> progDesc "Show text of a PDF file"
       <> header "hpdft - a PDF parsing tool" )

-- option parser

data CmdOpt = CmdOpt {
  page :: Int,
  ref  :: Int,
  refs :: Bool,
  pdftitle :: Bool,
  pdfinfo :: Bool,
  pdfoutline :: Bool,
  trailer :: Bool,
  file :: FilePath
  }

options :: Parser CmdOpt
options = CmdOpt
          <$> option auto
          ( long "page"
            <> short 'p'
            <> value 0
            <> metavar "PAGE"
            <> help "Page number (nomble)" )
          <*> option auto
          ( long "ref"
            <> short 'r'
            <> value 0
            <> metavar "REF"
            <> help "Object reference" )
          <*> switch
          ( long "refs"
            <> short 'R'
            <> help "Show object references in page order" )
          <*> switch
          ( long "title"
            <> short 'T'
            <> help "Show title (from metadata)" )
          <*> switch
          ( long "info"
            <> short 'I'
            <> help "Show PDF metainfo" )
          <*> switch
          ( long "toc"
            <> short 'O'
            <> help "Show table of contents (from metadata) " )
          <*> switch
          ( long "trailer"
            <> help "Show the trailer of PDF" )
          <*> strArgument
          ( help "input pdf file"
            <> metavar "FILE"
            <> action "file" )

hpdft :: CmdOpt -> IO ()
hpdft (CmdOpt 0 0 False False False False False fn) = pdfToText fn  
hpdft (CmdOpt 0 0 False True _ _ _ fn) = showTitle fn
hpdft (CmdOpt 0 0 False _ True _ _ fn) = showInfo fn
hpdft (CmdOpt 0 0 False _ _ True _ fn) = showOutlines fn
hpdft (CmdOpt 0 0 False _ _ _ True fn) = print =<< getTrailer fn
hpdft (CmdOpt 0 0 True _ _ _ _ fn) = print =<< refByPage fn
hpdft (CmdOpt n 0 False _ _ _ _ fn) = showPage fn n
hpdft (CmdOpt 0 r False _ _ _ _ fn) = BSL.putStr =<< getStreamByRef r =<< getPDFObjFromFile fn
hpdft _ = return ()

-- | Get a whole text from 'filename'. It works as:
--    (1) grub objects
--    (2) parse within each object, deflating its stream
--    (3) linearize stream

pdfToText filename = do
  objs <- getPDFObjFromFile filename
  rootref <- getRootRef filename
  BSL.putStrLn $ walkdown initstate rootref objs

walkdown :: PSR -> Int -> [PDFObj] -> PDFStream
walkdown st parent objs = 
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of 
        Just pr -> walkdown st pr objs
        Nothing -> ""
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kidsrefs -> BSL.concat $ map ((\f -> f objs) . (walkdown st)) kidsrefs
          Nothing -> ""
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> contentsStream dict st objs
          Nothing -> ""
    Nothing -> ""

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
      Just dict -> case findPages dict of 
        Just pr -> pageorder pr objs
        Nothing -> Nop
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
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

-- | Show contents of page 'page' in 'filename'.

showPage filename page = do 
  pagetree <- refByPage filename
  contentByRef filename $ pagetree !! (page - 1)

-- | Show /Content referenced from the 'ref'ed-object in 'filename'.

contentByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- getObjectByRef ref objs
  BSL.putStrLn $ contentInObject obj objs
  where contentInObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> contentsStream dict initstate objs
            Nothing -> ""

-- | Show /Title from meta information in 'filename'

showTitle filename = do
  d <- getInfo filename
  let title = 
        case findObjFromDict d "/Title" of
          Just (PdfText s) -> s
          Just x -> show x
          Nothing -> "No title anyway"
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

