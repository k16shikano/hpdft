{-# LANGUAGE OverloadedStrings #-}

module Cli.Grep (grepPDF) where

import Cli.Common (runOrDie, withFile)

import PDF.Document (Document(..), docRootRef, openDocument)
import PDF.Error (renderPdfError)
import PDF.Layout (LayoutOptions, defaultLayoutOptions)
import PDF.Page (pageRefsFromRoot)
import PDF.Text (pageTextGeomWith)

import System.IO (hPutStrLn, putStrLn, stderr)

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text.Lazy as TL (unpack)
import Data.Text.Lazy.Encoding as TL
import Control.Monad (when)

import Text.Regex.Base.RegexLike (makeRegex)
import Text.Regex.TDFA.String (regexec)

grepPDF :: LayoutOptions -> FilePath -> Maybe String -> String -> IO ()
grepPDF lopts filename mpw re =
  withFile filename $ do
    doc <- runOrDie (openDocument filename mpw)
    root <- runOrDie (return (docRootRef doc))
    let objs = docObjs doc
        refs = pageRefsFromRoot root objs
    mapM_
      (\(ref, pagenm) -> grepByPage doc lopts pagenm re ref)
      (zip refs [1 ..])

grepByPage :: Document -> LayoutOptions -> Int -> String -> Int -> IO ()
grepByPage doc lopts pagenm re ref =
  case pageTextGeomWith lopts doc ref of
    Right txt -> do
      let matched = filter (not . null) $ map (grepByLine re) (BSL.lines txt)
      when (not (null matched)) (showResult pagenm matched)
    Left err ->
      hPutStrLn stderr ("hpdft: page " ++ show pagenm ++ ": " ++ renderPdfError err)

showResult :: Int -> [String] -> IO ()
showResult p m = do
  putStrLn $ "At page " <> show p <> "..."
  mapM_ (putStrLn . (" | " <>)) m

grepByLine :: String -> BSL.ByteString -> String
grepByLine re txt =
  case regexec (makeRegex re) (TL.unpack (TL.decodeUtf8 txt)) of
    Left _  -> ""
    Right m -> case m of
      Just (b, m', a, _) -> b <> highlight m' <> a
      Nothing            -> ""
  where
    highlight x = "\ESC[31m" <> x <> "\ESC[0m"
