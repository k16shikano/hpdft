module Main where

import PDF.Definition
import PDF.Document (Document(..), docRootRef, openDocument)
import PDF.DocumentStructure (findDictOfType, findKids, findObjsByRef, findPages)
import PDF.Error (PdfError(..), PdfResult)
import PDF.Interpret (Glyph(..), PageItem(..), Rect(..), interpretPageItems)

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
  root <- runOrDie (docRootRef doc)
  let refs = pageRefs root (docObjs doc)
  if pageNo < 1 || pageNo > length refs
    then putStrLn "page out of range" >> exitFailure
    else do
      let pageRef = refs !! (pageNo - 1)
      items <- runOrDie (interpretPageItems doc pageRef)
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
          ++ " wm=" ++ show (glyphWMode g) ++ " font=" ++ glyphFont g
        ItemGraphic r -> putStrLn $
          "R x0=" ++ show (rectX0 r) ++ " y0=" ++ show (rectY0 r)
          ++ " x1=" ++ show (rectX1 r) ++ " y1=" ++ show (rectY1 r)

pageRefs :: Int -> PDFObjIndex -> [Int]
pageRefs parent objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> pageRefs pr objs
        Nothing -> []
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kids -> concatMap (\k -> pageRefs k objs) kids
          Nothing -> []
        Nothing -> case findDictOfType "/Page" os of
          Just _ -> [parent]
          Nothing -> []
    Nothing -> []

runOrDie :: PdfResult a -> IO a
runOrDie (Right x) = return x
runOrDie (Left err) = print err >> exitFailure
