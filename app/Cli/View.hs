{-# LANGUAGE OverloadedStrings #-}

module Cli.View (runViewer) where

import Cli.Common (runOrDie, withFile)
import Cli.Text (streamLegacyToStdout)

import PDF.Document (openDocument)
import TuiGeometry (parseHeightSpec)
import TuiPreview (runTuiPreview)

import System.Exit (exitWith, ExitCode(..))
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

runViewer :: FilePath -> Maybe String -> Maybe String -> IO ()
runViewer filename mpw mHeightStr = do
  mHeight <- case mHeightStr of
    Nothing -> return Nothing
    Just s ->
      case parseHeightSpec s of
        Nothing -> do
          hPutStrLn stderr "hpdft: invalid --height value (use N or N%)"
          exitWith (ExitFailure 1)
        Just spec -> return (Just spec)
  doc <- runOrDie (openDocument filename mpw)
  stdoutTTY <- hIsTerminalDevice stdout
  if stdoutTTY
    then runTuiPreview filename doc mHeight
    else streamLegacyToStdout doc False
