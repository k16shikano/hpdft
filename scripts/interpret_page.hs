#!/usr/bin/env cabal
{- cabal:
build-depends: base, hpdft, text
-}

module Main where

import PDF.Definition
import PDF.Document (Document(..), docRootRef, openDocument)
import PDF.DocumentStructure (findDictOfType, findKids, findObjsByRef, findPages)
import PDF.Error (PdfError(..), PdfResult)
import PDF.Interpret (Glyph(..), PageItem(..), interpretPageItems)

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  let pdf = case args of
        (f:_) -> f
        _     -> "data/sample/test.pdf"
  result <- openDocument pdf Nothing
  case result of
    Left err -> do
      putStrLn $ show err
      exitFailure
    Right doc -> inspect doc

inspect :: Document -> IO ()
inspect doc = do
  root <- runOrDie (docRootRef doc)
  pageRef <- runOrDie (firstPageRef root (docObjs doc))
  items <- runOrDie (interpretPageItems doc pageRef)
  let glyphs = [g | ItemGlyph g <- items]
      graphics = length [() | ItemGraphic _ <- items]
  putStrLn $ "page " ++ show pageRef ++ ": " ++ show (length glyphs) ++ " glyph segments, " ++ show graphics ++ " graphics"
  forM_ (take 10 glyphs) $ \g ->
    putStrLn $ show (glyphText g) ++ " @ (" ++ show (glyphX g) ++ ", " ++ show (glyphY g) ++ ") size=" ++ show (glyphSize g)

firstPageRef :: Int -> PDFObjIndex -> PdfResult Int
firstPageRef parent objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> firstPageRef pr objs
        Nothing -> Left (MissingKey "/Pages" "catalog")
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just (kid:_) -> firstPageRef kid objs
          Nothing -> Left (MissingKey "/Kids" "pages")
        Nothing -> case findDictOfType "/Page" os of
          Just _ -> Right parent
          Nothing -> Left (MissingKey "/Type" "page tree")
    Nothing -> Left (MissingObject parent)

runOrDie :: PdfResult a -> IO a
runOrDie (Right x) = return x
runOrDie (Left err) = do
  putStrLn $ show err
  exitFailure
