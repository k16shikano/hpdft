{-# LANGUAGE OverloadedStrings #-}

module Cli.Misc
  ( runExtractImages
  , runExtractForm
  , runDiff
  , showRefs
  , showContent
  , showTitle
  , showInfo
  , showOutlines
  , showTrailer
  ) where

import Cli.Common (maybePassword, runOrDie, withFile)
import Cli.Parser (DiffOpt(..), FormOpt(..), ImagesOpt(..))

import PDF.Definition (Obj(..), ppObj, ppDictEntries)
import PDF.Document (Document(..), docInfoDict, docRootRef, docTrailer, openDocument)
import PDF.DocumentStructure
import PDF.Diff (TextChange(..), compareDocuments)
import PDF.FormExtract (extractFormToFile, pageFormNames)
import PDF.Image (extractPageImagesToDir)
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions)
import PDF.Outlines (getOutlines)
import PDF.Page (pageRefsFromRoot)
import PDF.PDFIO (getObjectByRef, getStream)

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, putStrLn, stderr)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.List (find, intercalate)

runExtractImages :: ImagesOpt -> IO ()
runExtractImages ImagesOpt{ioPage=pg, ioOut=out, ioPassword=pw, ioFile=fn} =
  withFile fn $
  if pg < 1
    then do
      hPutStrLn stderr "hpdft: image requires -p PAGE (1-based)"
      exitWith (ExitFailure 1)
    else do
      doc <- runOrDie (openDocument fn (maybePassword pw))
      paths <- runOrDie (extractPageImagesToDir doc pg out)
      if null paths
        then hPutStrLn stderr ("hpdft: no image XObjects on page " ++ show pg)
        else mapM_ putStrLn paths

runExtractForm :: FormOpt -> IO ()
runExtractForm FormOpt{foPage=pg, foName=mn, foOut=out, foPassword=pw, foFile=fn} =
  withFile fn $
  if pg < 1
    then do
      hPutStrLn stderr "hpdft: form requires -p PAGE (1-based)"
      exitWith (ExitFailure 1)
    else do
      doc <- runOrDie (openDocument fn (maybePassword pw))
      case mn of
        Nothing -> do
          names <- runOrDie (return (pageFormNames doc pg))
          if null names
            then hPutStrLn stderr ("hpdft: no Form XObjects on page " ++ show pg)
            else mapM_ (putStrLn . T.unpack) names
        Just nm -> do
          path <- runOrDie (extractFormToFile doc pg (T.pack nm) out)
          putStrLn path

runDiff :: DiffOpt -> IO ()
runDiff DiffOpt{doRuby=rb, doJson=json, doPassword=pw, doFileA=fa, doFileB=fb} =
  withFile fa $
  withFile fb $
  let mpw = maybePassword pw
      lopts = defaultLayoutOptions {optRuby = rb}
  in do
    docA <- runOrDie (openDocument fa mpw)
    docB <- runOrDie (openDocument fb mpw)
    changes <- runOrDie (return (compareDocuments lopts docA docB))
    if json
      then putStrLn (renderDiffJson changes)
      else mapM_ putStrLn (renderDiffHuman changes)

renderDiffHuman :: [TextChange] -> [String]
renderDiffHuman = map renderOne
  where
    renderOne (PageCountMismatch pa pb) =
      "page count mismatch: " ++ show pa ++ " vs " ++ show pb
    renderOne TextChange{changePageA = pa, changePageB = pb,
                         changeParaA = pxa, changeParaB = pxb,
                         changeOld = old, changeNew = new} =
      unlines
        ( pageLine
        : paraLine
        : ("- old: " ++ T.unpack old) : ("+ new: " ++ T.unpack new) : []
        )
      where
        pageLine =
          case (pa, pb) of
            (Just a, Just b) | a == b -> "page " ++ show a ++ ":"
            (Just a, Just b) -> "page " ++ show a ++ " vs " ++ show b ++ ":"
            (Just a, Nothing) -> "page " ++ show a ++ " (only in first file):"
            (Nothing, Just b) -> "page " ++ show b ++ " (only in second file):"
            _ -> "page ?:"
        paraLine =
          case (pxa, pxb) of
            (Just a, Just b) | a == b -> "para " ++ show (a + 1) ++ ":"
            (Just a, Just b) -> "para " ++ show (a + 1) ++ " vs " ++ show (b + 1) ++ ":"
            (Just a, Nothing) -> "para " ++ show (a + 1) ++ ":"
            (Nothing, Just b) -> "para " ++ show (b + 1) ++ ":"
            _ -> "para ?:"

renderDiffJson :: [TextChange] -> String
renderDiffJson changes = "[" ++ intercalate "," (map encodeChange changes) ++ "]"
  where
    encodeChange (PageCountMismatch pa pb) =
      "{\"type\":\"pageCountMismatch\",\"pagesA\":" ++ show pa
        ++ ",\"pagesB\":" ++ show pb ++ "}"
    encodeChange TextChange{changePageA = pa, changePageB = pb,
                            changeParaA = pxa, changeParaB = pxb,
                            changeOld = old, changeNew = new} =
      "{\"type\":\"textChange\""
        ++ maybeField "pageA" pa
        ++ maybeField "pageB" pb
        ++ maybeField "paraA" pxa
        ++ maybeField "paraB" pxb
        ++ ",\"old\":" ++ jsonString old
        ++ ",\"new\":" ++ jsonString new
        ++ "}"
    maybeField _ Nothing = ""
    maybeField k (Just v) = ",\"" ++ k ++ "\":" ++ show v
    jsonString t =
      "\"" ++ concatMap esc (T.unpack t) ++ "\""
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c = [c]

showRefs :: FilePath -> Maybe String -> IO ()
showRefs filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  root <- runOrDie (return (docRootRef doc))
  print $ pageRefsFromRoot root (docObjs doc)

showContent :: FilePath -> Maybe String -> Int -> IO ()
showContent filename mpw ref = do
  doc <- runOrDie (openDocument filename mpw)
  let objs = docObjs doc
      sec = docSecurity doc
  obj <- runOrDie (getObjectByRef ref objs)
  if hasStream obj
    then case findDict obj of
      Just d | hasSubtype d -> printStreamWithDict sec ref d obj
      _ -> do
        strm <- runOrDie (getStream sec ref False obj)
        BSL.putStrLn strm
    else do
      objs' <- runOrDie (getObjectByRef ref objs)
      putStrLn $ "[" ++ intercalate ", " (map ppObj objs') ++ "]"
  where
    hasStream obj = case find isStream obj of
      Just _ -> True
      Nothing -> False
    isStream (PdfStream _) = True
    isStream _             = False
    hasSubtype d = case findObjFromDict d "/Subtype" of
      Just _ -> True
      Nothing -> False
    printStreamWithDict sec' ref' d obj = do
      putStrLn $ ppObj (PdfDict d)
      strm <- runOrDie (getStream sec' ref' True obj)
      BSL.putStrLn strm

showTitle :: FilePath -> Maybe String -> IO ()
showTitle filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  d <- runOrDie (return (docInfoDict doc))
  let title =
        case findObjFromDict d "/Title" of
          Just (PdfText s) -> T.unpack s
          Just x -> ppObj x
          Nothing -> "No title anyway"
  putStrLn title

showInfo :: FilePath -> Maybe String -> IO ()
showInfo filename mpw = do
  doc <- runOrDie (openDocument filename mpw)
  d <- runOrDie (return (docInfoDict doc))
  putStrLn $ ppObj (PdfDict d)

showOutlines :: FilePath -> Maybe String -> IO ()
showOutlines filename mpw = do
  d <- runOrDie (getOutlines filename mpw)
  putStrLn $ show d

showTrailer :: FilePath -> IO ()
showTrailer filename = do
  doc <- runOrDie (openDocument filename Nothing)
  putStrLn $ ppDictEntries (docTrailer doc)
