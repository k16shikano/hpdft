{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
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
import Data.Text.Lazy as TL (unpack)
import Data.Text.Lazy.Encoding as TL
import Data.List (nub, find)
import Data.Maybe (fromMaybe)

import Options.Applicative
import Data.Semigroup ((<>))

import Text.Regex.Base.RegexLike ( makeRegex )
import Text.Regex.TDFA.String    ( regexec )

import Options.Applicative (strOption)
import Control.Monad (when)
import PDF.Definition (Obj(PdfStream))

import qualified Paths_hpdft as Autogen (version)
import Data.Version (showVersion)

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
    opts = info (options <**> helper <**> (simpleVersioner versionInfo))
      (fullDesc
       <> header versionInfo)

versionInfo = "hpdft - a PDF to text converter, version " <> showVersion Autogen.version

-- option parser

data CmdOpt = CmdOpt {
  page :: Int,
  ref  :: Int,
  grep :: String,
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
          <*> strOption
          ( long "grep"
            <> short 'g'
            <> value ""
            <> metavar "RegExp"
            <> help "grep PDF" )
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
hpdft (CmdOpt 0 0 "" False False False False False fn) = pdfToText fn  
hpdft (CmdOpt 0 0 "" False True _ _ _ fn) = showTitle fn
hpdft (CmdOpt 0 0 "" False _ True _ _ fn) = showInfo fn
hpdft (CmdOpt 0 0 "" False _ _ True _ fn) = showOutlines fn
hpdft (CmdOpt 0 0 "" False _ _ _ True fn) = print =<< getTrailer fn
hpdft (CmdOpt 0 0 "" True  _ _ _ _ fn) = print =<< refByPage fn
hpdft (CmdOpt n 0 "" False _ _ _ _ fn) = showPage fn n
hpdft (CmdOpt 0 r "" False _ _ _ _ fn) = showContent fn r
hpdft (CmdOpt 0 0 r  False _ _ _ _ fn) = grepPDF fn r
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
  case length pagetree >= page of
    True -> contentByRef filename $ pagetree !! (page - 1)
    False -> putStrLn $ "hpdft: No Page "++(show page)

-- | Show /Content referenced from the 'ref'ed-object in 'filename'.

contentByRef filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- getObjectByRef ref objs
  BSL.putStrLn $ contentInObject obj objs
  where contentInObject obj objs =
          case findDictOfType "/Page" obj of
            Just dict -> contentsStream dict initstate objs
            Nothing -> ""

showContent filename ref = do
  objs <- getPDFObjFromFile filename
  obj <- getObjectByRef ref objs
  let d = fromMaybe [] $ findDict obj
  if hasStream obj
    then
      if hasSubtype d -- then it's not content stream
        then printStreamWithDict d obj
        else BSL.putStrLn =<< getStream False obj
    else print =<< getObjectByRef ref objs
  where

    hasStream obj = case find isStream obj of
      Just _ -> True
      Nothing -> False
    isStream (PdfStream _) = True
    isStream _             = False

    hasSubtype d = case find isSubtype d of
                       Just _ -> True
                       Nothing -> False
    isSubtype (PdfName "/Subtype", _) = True
    isSubtype x = False

    printStreamWithDict :: Dict -> [Obj] -> IO ()
    printStreamWithDict d obj = print (PdfDict d) >> (BSL.putStrLn =<< getStream True obj)

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

-- | Find string in each page.

grepPDF filename re = do
  root <- getRootRef filename
  objs <- getPDFObjFromFile filename
  mapM
    (\(strm, pagenm) -> (grepByPage pagenm re . contentInObjs objs) strm)
    $ zip (pageTreeToList $ pageorder root objs) [1..]
  return ()
  
  where
    contentInObjs objs ref =
      case findObjsByRef ref objs of
        Just obj -> case findDictOfType "/Page" obj of
                      Just dict -> contentsStream dict initstate objs
                      Nothing -> ""
        Nothing -> error $ "No Object with Ref " ++ show ref

    grepByPage :: Int -> String -> PDFStream -> IO ()
    grepByPage pagenm re txt = do
      let matched = filter (not . null) $ map (grepByLine re) $ BSL.lines txt
      when (not $ null matched) (showResult pagenm matched)
      return ()
      where
        showResult p m = do
          putStrLn $ "At page " <> show p <> "..."
          mapM (putStrLn . (" | " <>)) m
          return ()

    grepByLine :: String -> PDFStream -> String
    grepByLine re txt =
      case regexec (makeRegex re) $ TL.unpack $ TL.decodeUtf8 txt of
        Left _  -> ""
        Right m -> case m of
         Just (b, m, a, _) -> (b <> (highlight m) <> a)
         Nothing           -> ""

    highlight m = "\ESC[31m" <> m <> "\ESC[0m"

