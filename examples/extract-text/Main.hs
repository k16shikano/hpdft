-- | Minimal text extraction example for hpdft.
--
-- Usage:
--
-- @
-- cabal run extract-text -- path/to/file.pdf
-- cabal run extract-text
--   -- uses data/fixtures/classic.pdf when run from the repo tree
-- @
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Document (openDocument)
import PDF.Text (pdfToTextTaggedDoc)

import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)

import qualified Data.ByteString.Lazy.Char8 as BSL

main :: IO ()
main = do
  args <- getArgs
  pdf <- case args of
    (f : _) -> return f
    _       -> defaultFixture
  result <- openDocument pdf Nothing
  case result of
    Left err -> print err >> exitFailure
    Right doc -> case pdfToTextTaggedDoc doc of
      Left err -> print err >> exitFailure
      Right bs -> BSL.putStr bs

defaultFixture :: IO FilePath
defaultFixture = findRepoFile "data/fixtures/classic.pdf"

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
                   "usage: extract-text [FILE.pdf]\n"
                     ++ "  (default fixture not found: "
                     ++ rel
                     ++ ")"
                 exitFailure
               else go parent
