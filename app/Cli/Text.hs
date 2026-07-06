{-# LANGUAGE OverloadedStrings #-}

module Cli.Text
  ( runExtractText
  , streamLegacyToStdout
  , showPage
  ) where

import Cli.Common (maybePassword, printWarnings, runOrDie, withFile)
import Cli.Parser (ExtractOpt(..))

import PDF.Document (Document(..), docRootRef, openDocument)
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions)
import PDF.Page (pageRefsFromRoot)
import PDF.Text
  ( pageTextGeomWith
  , pdfToTextGeomBSWith
  , pdfToTextStreamDoc
  , pdfToTextTaggedBSWith
  )

import System.IO (hFlush, hIsTerminalDevice, hPutStr, hPutStrLn, putStrLn, stderr, stdout)

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad (when)

runExtractText :: ExtractOpt -> IO ()
runExtractText ExtractOpt{eoPage=pg, eoGeom=gm, eoTagged=tg, eoLegacy=lg,
                      eoQuiet=quiet, eoFootnotes=fnn, eoRuby=rb,
                      eoPassword=pw, eoFile=fn} =
  withFile fn $
  let mpw = maybePassword pw
      lopts = defaultLayoutOptions {optFootnotes = fnn, optRuby = rb}
  in if pg /= 0
     then showPage lopts fn mpw pg
     else case () of
       _ | lg && not gm && not tg -> do
             doc <- runOrDie (openDocument fn mpw)
             streamLegacyToStdout doc quiet
         | gm && not tg && not lg -> pdfToTextGeom lopts fn mpw
         | otherwise              -> pdfToTextTagged lopts fn mpw

streamLegacyToStdout :: Document -> Bool -> IO ()
streamLegacyToStdout doc quiet = do
  stderrTTY <- hIsTerminalDevice stderr
  let showProgress pg total =
        when (not quiet && stderrTTY && total > 0) $
          hPutStr stderr ("\rhpdft: page " ++ show pg ++ "/" ++ show total ++ "...")
      clearProgress total =
        when (not quiet && stderrTTY && total > 0) $
          hPutStr stderr ("\r\ESC[K")
  totalRef <- newIORef (0 :: Int)
  ws <- pdfToTextStreamDoc doc $ \pg total txt -> do
    writeIORef totalRef total
    showProgress pg total
    BSL.putStr txt
    hFlush stdout
  total <- readIORef totalRef
  clearProgress total
  putStrLn ""
  printWarnings ws

pdfToTextGeom :: LayoutOptions -> FilePath -> Maybe String -> IO ()
pdfToTextGeom lopts filename mpw = do
  txt <- runOrDie (pdfToTextGeomBSWith lopts filename mpw)
  BSL.putStrLn txt

pdfToTextTagged :: LayoutOptions -> FilePath -> Maybe String -> IO ()
pdfToTextTagged lopts filename mpw = do
  txt <- runOrDie (pdfToTextTaggedBSWith lopts filename mpw)
  BSL.putStrLn txt

showPage :: LayoutOptions -> FilePath -> Maybe String -> Int -> IO ()
showPage lopts filename mpw page = do
  doc <- runOrDie (openDocument filename mpw)
  root <- runOrDie (return (docRootRef doc))
  let pagetree = pageRefsFromRoot root (docObjs doc)
  if page >= 1 && length pagetree >= page
    then do
      txt <- runOrDie (return (pageTextGeomWith lopts doc (pagetree !! (page - 1))))
      BSL.putStr txt
    else putStrLn $ "hpdft: No Page " ++ show page
