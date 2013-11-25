{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Directory

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Pdf
import PdfObj
import PdfStream

-- First: grub objects
-- Second: parse within each object, deflating its stream
-- Third: linearize stream

main = do
  (fileName:_) <- getArgs
  contents <- BS.readFile fileName
  let objs = map parsePDFObj $ getObjs contents
  let root = case rootRef contents of
        Just r  -> r
        Nothing -> 0
--  putStrLn $ show $ grubFontDiff 1666 objs
--  putStrLn $ show $ parsePDFObj (getObjs contents !! 2)
--  BSL.putStrLn $ decompressStream $ (getObjs contents) !! 2
  BSL.putStrLn $ linearize root objs


-- linearize objects

linearize :: Int -> [PDFObj] -> PDFStream
linearize parent objs = 
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case pages dict of 
        Just pr -> linearize pr objs
        Nothing -> ""
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case pagesKids dict of
          Just kidsrefs -> BSL.concat $ map (\f -> f objs) (map linearize kidsrefs)
          Nothing -> ""
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> contentsStream dict initstate objs
          Nothing -> ""
    Nothing -> ""

--initstate = (0,0,70,660)
initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=700
                , leftmargin=0.0
                , top=700.0
                , bottom=0.0
                , curfont=""
                , fontmaps=[]}

