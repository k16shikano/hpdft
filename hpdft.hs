{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Definition

import PDF.Object
import PDF.DocumentStructure
import PDF.PDFIO
import PDF.Outlines
import PDF.Encrypt (Security)
import PDF.Text (initstate, pdfToTextBS)

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
  password :: String,
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
          <*> strOption
          ( long "password"
            <> short 'P'
            <> value ""
            <> metavar "PASSWORD"
            <> help "Password for encrypted PDF" )
          <*> strArgument
          ( help "input pdf file"
            <> metavar "FILE"
            <> action "file" )

hpdft :: CmdOpt -> IO ()
hpdft CmdOpt{password=pw, file=fn, page=pg, ref=rf, grep=gr, refs=rs, pdftitle=tt, pdfinfo=ii, pdfoutline=oo, trailer=tr} =
  let mpw = Just pw
  in case () of
    _ | pg==0 && rf==0 && null gr && not rs && not tt && not ii && not oo && not tr -> pdfToText fn mpw
      | pg==0 && rf==0 && null gr && not rs && tt      -> showTitle fn mpw
      | pg==0 && rf==0 && null gr && not rs && ii      -> showInfo fn mpw
      | pg==0 && rf==0 && null gr && not rs && oo      -> showOutlines fn mpw
      | pg==0 && rf==0 && null gr && not rs && tr      -> print =<< getTrailer fn
      | pg==0 && rf==0 && null gr && rs                -> print =<< refByPage fn mpw
      | rf==0 && null gr && pg/=0                      -> showPage fn mpw pg
      | pg==0 && null gr && rf/=0                      -> showContent fn mpw rf
      | pg==0 && rf==0 && not (null gr)                -> grepPDF fn mpw gr
      | otherwise -> return ()

-- | Get a whole text from 'filename'. It works as:
--    (1) grub objects
--    (2) parse within each object, deflating its stream
--    (3) linearize stream

pdfToText filename mpw = do
  txt <- pdfToTextBS filename mpw
  BSL.putStrLn txt

data  PageTree = Nop | Page Int | Pages [PageTree]
                 deriving Show

-- | Sort object references in page order.

refByPage filename mpw = do
  root <- getRootRef filename
  (objs, _) <- getPDFObjFromFile filename mpw
  return $ pageTreeToList $ pageorder root objs

pageorder :: Int -> PDFObjIndex -> PageTree
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

showPage filename mpw page = do
  (objs, sec) <- getPDFObjFromFile filename mpw
  root <- getRootRef filename
  let pagetree = pageTreeToList $ pageorder root objs
  case length pagetree >= page of
    True -> contentByRefObjs sec objs $ pagetree !! (page - 1)
    False -> putStrLn $ "hpdft: No Page "++(show page)

contentByRefObjs sec objs ref = do
  obj <- getObjectByRef ref objs
  BSL.putStrLn $ contentInObject sec obj objs
  where contentInObject sec' obj' objs' =
          case findDictOfType "/Page" obj' of
            Just dict -> contentsStream dict initstate sec' objs'
            Nothing -> ""

contentByRef filename mpw ref = do
  (objs, sec) <- getPDFObjFromFile filename mpw
  contentByRefObjs sec objs ref

showContent filename mpw ref = do
  (objs, sec) <- getPDFObjFromFile filename mpw
  obj <- getObjectByRef ref objs
  let d = fromMaybe [] $ findDict obj
  if hasStream obj
    then
      if hasSubtype d
        then printStreamWithDict sec ref d obj
        else BSL.putStrLn =<< getStream sec ref False obj
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

    printStreamWithDict :: Maybe Security -> Int -> Dict -> [Obj] -> IO ()
    printStreamWithDict sec' ref' d obj =
      print (PdfDict d) >> (BSL.putStrLn =<< getStream sec' ref' True obj)

-- | Show /Title from meta information in 'filename'

showTitle filename mpw = do
  d <- getInfo filename mpw
  let title = 
        case findObjFromDict d "/Title" of
          Just (PdfText s) -> s
          Just x -> show x
          Nothing -> "No title anyway"
  putStrLn title
  return ()

-- | Show /Info from meta information in 'filename'

showInfo filename mpw = do
  d <- getInfo filename mpw
  putStrLn $ toString 0 (PdfDict d)
  return ()

-- | Show /Outlines from meta information in 'filename'

showOutlines filename mpw = do
  d <- getOutlines filename mpw
  putStrLn $ show d
  return ()

-- | Find string in each page.

grepPDF filename mpw re = do
  root <- getRootRef filename
  (objs, sec) <- getPDFObjFromFile filename mpw
  mapM
    (\(strm, pagenm) -> (grepByPage pagenm re . contentInObjs sec objs) strm)
    $ zip (pageTreeToList $ pageorder root objs) [1..]
  return ()
  
  where
    contentInObjs sec' objs' ref =
      case findObjsByRef ref objs' of
        Just obj -> case findDictOfType "/Page" obj of
                      Just dict -> contentsStream dict initstate sec' objs'
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

