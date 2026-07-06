{-# LANGUAGE OverloadedStrings #-}

module Cli.Common
  ( maybePassword
  , withFile
  , runOrDie
  , printWarnings
  ) where

import PDF.Error (PdfError(..), PdfResult, PdfWarning(..), renderPdfError, renderPdfWarning)

import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, IOException, ioError)

maybePassword :: String -> Maybe String
maybePassword pw = if null pw then Nothing else Just pw

withFile :: FilePath -> IO () -> IO ()
withFile fp action =
  action `catch` \e -> do
    if isDoesNotExistError (e :: IOException)
      then do
        hPutStrLn stderr ("hpdft: " ++ fp ++ ": does not exist")
        exitWith (ExitFailure 1)
      else ioError e

describeError :: PdfError -> String
describeError err =
  case err of
    DecryptionError msg ->
      "cannot decrypt: " ++ msg ++ ". Use -P to supply a password."
    _ -> renderPdfError err

runOrDie :: IO (PdfResult a) -> IO a
runOrDie action = do
  result <- action
  case result of
    Right a -> return a
    Left err -> do
      hPutStrLn stderr ("hpdft: " ++ describeError err)
      exitWith (ExitFailure 1)

printWarnings :: [PdfWarning] -> IO ()
printWarnings = mapM_ (hPutStrLn stderr . ("hpdft: warning: " ++) . renderPdfWarning)
