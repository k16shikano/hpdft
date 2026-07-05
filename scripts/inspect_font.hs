#!/usr/bin/env cabal
{- cabal:
build-depends: base, containers, hpdft
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Document (Document(..), docRootRef, openDocument)
import PDF.DocumentStructure
  ( findDictByRef
  , findDictOfType
  , findKids
  , findObjFromDict
  , findObjsByRef
  , findPages
  , fontInfo
  )
import PDF.Definition (FontInfo(..), Obj(..), PDFObjIndex)
import PDF.Error (PdfError(..), PdfResult)

import qualified Data.Map as M

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  let pdf = case args of
        (f:_) -> f
        _ -> "data/sample/test.pdf"
  result <- openDocument pdf Nothing
  case result of
    Left err -> do
      putStrLn $ show err
      exitFailure
    Right doc -> inspect doc

inspect :: Document -> IO ()
inspect doc = do
  root <- runOrDie (docRootRef doc)
  let objs = docObjs doc
      sec = docSecurity doc
  pageRef <- runOrDie (firstPageRef root objs)
  case findObjsByRef pageRef objs of
    Just os -> case findDictOfType "/Page" os of
      Just pageDict -> do
        let fonts = pageFontRefs pageDict objs
        putStrLn $ "page " ++ show pageRef ++ ", fonts: " ++ show (length fonts)
        forM_ fonts $ \(name, ref) -> do
          let fi = fontInfo sec ref objs
          putStrLn $ "--- " ++ name ++ " (obj " ++ show ref ++ ") ---"
          putStrLn $ "fiBytesPerCode = " ++ show (fiBytesPerCode fi)
          putStrLn $ "fiWMode = " ++ show (fiWMode fi)
          putStrLn $ "fiDefaultWidth = " ++ show (fiDefaultWidth fi)
          forM_ sampleCodes $ \code ->
            putStrLn $ "  fiWidth " ++ show code ++ " = " ++ show (fiWidth fi code)
      _ -> putStrLn "no page dict"
    _ -> putStrLn "no page object"

sampleCodes :: [Int]
sampleCodes = [32, 65, 66, 67, 97, 98, 99]

pageFontRefs :: M.Map String Obj -> PDFObjIndex -> [(String, Int)]
pageFontRefs pageDict objs =
  case pageResources pageDict objs of
    Just res -> case findObjFromDict res "/Font" of
      Just (PdfDict fd) -> fontRefList fd
      Just (ObjRef x) -> case findDictByRef x objs of
        Just fd -> fontRefList fd
        _ -> []
      _ -> []
    _ -> []

pageResources :: M.Map String Obj -> PDFObjIndex -> Maybe (M.Map String Obj)
pageResources pageDict objs = case M.lookup "/Resources" pageDict of
  Just (ObjRef x) -> findDictByRef x objs
  Just (PdfDict d) -> Just d
  _ -> Nothing

fontRefList :: M.Map String Obj -> [(String, Int)]
fontRefList = map refEntry . filter isFontRef . M.toList
  where
    isFontRef (_, ObjRef _) = True
    isFontRef _ = False
    refEntry (n, ObjRef r) = (n, r)
    refEntry _ = ("?", 0)

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
