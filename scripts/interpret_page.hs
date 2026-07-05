{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Document (Document, openDocument)
import PDF.Error (PdfResult)
import PDF.Interpret (Glyph(..), PageItem(..), Rect(..))
import PDF.Page (pageCount, pageRefAt, pageItems)

import qualified Data.Text as T
import Control.Monad (forM_)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  let (pdf, pageNo, full) = case args of
        (f:n:m:_) -> (f, read n :: Int, m == "--full")
        (f:n:_)   -> (f, read n :: Int, False)
        (f:_)     -> (f, 1, False)
        _         -> ("data/sample/test.pdf", 1, False)
  result <- openDocument pdf Nothing
  case result of
    Left err -> print err >> exitFailure
    Right doc -> inspect doc pageNo full

inspect :: Document -> Int -> Bool -> IO ()
inspect doc pageNo full = do
  n <- runOrDie (pageCount doc)
  if pageNo < 1 || pageNo > n
    then putStrLn "page out of range" >> exitFailure
    else do
      pageRef <- runOrDie (pageRefAt doc pageNo)
      items <- runOrDie (pageItems doc pageRef)
      let glyphs = [g | ItemGlyph g <- items]
          graphics = length [() | ItemGraphic _ <- items]
      putStrLn $ "page ref " ++ show pageRef ++ ": "
        ++ show (length glyphs) ++ " glyph segments, "
        ++ show graphics ++ " graphics"
      let shown = if full then items else take 10 [i | i@(ItemGlyph _) <- items]
      forM_ shown $ \item -> case item of
        ItemGlyph g -> putStrLn $
          "G " ++ show (glyphText g)
          ++ " x=" ++ show (glyphX g) ++ " y=" ++ show (glyphY g)
          ++ " w=" ++ show (glyphWidth g) ++ " size=" ++ show (glyphSize g)
          ++ " wm=" ++ show (glyphWMode g) ++ " font=" ++ T.unpack (glyphFont g)
        ItemGraphic r -> putStrLn $
          "R x0=" ++ show (rectX0 r) ++ " y0=" ++ show (rectY0 r)
          ++ " x1=" ++ show (rectX1 r) ++ " y1=" ++ show (rectY1 r)

runOrDie :: PdfResult a -> IO a
runOrDie (Right x) = return x
runOrDie (Left err) = print err >> exitFailure
