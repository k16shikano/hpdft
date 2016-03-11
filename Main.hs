module Main where

import PDF.PDFIO
import System.Environment

main = do
  (filename:_) <- getArgs
  obj <- getPDFObjFromFile filename
  putStrLn $ show $ length obj
  return ()
