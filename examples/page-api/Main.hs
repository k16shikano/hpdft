-- | Page-level API example: count pages and print paragraph regions.
--
-- Usage:
--
-- @
-- cabal run page-api -- path/to/file.pdf
-- cabal run page-api
--   -- uses data/fixtures/paragraphs.pdf when run from the repo tree
-- @
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Document (openDocument)
import PDF.Error (PdfResult)
import PDF.Interpret (Rect(..))
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions)
import PDF.Page (PageRegion(..), pageCount, pageParagraphs, pageRefAt, pageRegions)

import Control.Monad (forM_)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)

import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  pdf <- case args of
    (f : _) -> return f
    _       -> defaultFixture
  result <- openDocument pdf Nothing
  case result of
    Left err -> print err >> exitFailure
    Right doc -> do
      n <- runOrDie (pageCount doc)
      putStrLn $ "pages: " ++ show n
      let opts = defaultLayoutOptions {optRuby = False}
      forM_ [1 .. n] $ \pageNo -> do
        ref <- runOrDie (pageRefAt doc pageNo)
        paras <- runOrDie (pageParagraphs doc ref opts)
        regions <- runOrDie (pageRegions doc ref opts)
        putStrLn $ "--- page " ++ show pageNo ++ " (" ++ show (length paras) ++ " paragraphs) ---"
        forM_ regions $ \PageRegion{regionParagraph = idx, regionBBox = bbox, regionText = txt} ->
          putStrLn $
            "  ["
              ++ show idx
              ++ "] "
              ++ showRect bbox
              ++ ": "
              ++ T.unpack (T.take 80 txt)

defaultFixture :: IO FilePath
defaultFixture = findRepoFile "data/fixtures/paragraphs.pdf"

findRepoFile :: FilePath -> IO FilePath
findRepoFile rel = do
  cwd <- getCurrentDirectory
  go cwd
  where
    go dir = do
      let candidate = dir </> rel
      exists <- doesFileExist candidate
      if exists
        then return candidate
        else
          let parent = takeDirectory dir
          in if parent == dir
               then do
                 putStrLn $
                   "usage: page-api [FILE.pdf]\n"
                     ++ "  (default fixture not found: "
                     ++ rel
                     ++ ")"
                 exitFailure
               else go parent

showRect :: Rect -> String
showRect r =
  "("
    ++ show (rectX0 r)
    ++ ","
    ++ show (rectY0 r)
    ++ ")-("
    ++ show (rectX1 r)
    ++ ","
    ++ show (rectY1 r)
    ++ ")"

runOrDie :: PdfResult a -> IO a
runOrDie (Right x) = return x
runOrDie (Left err) = print err >> exitFailure
