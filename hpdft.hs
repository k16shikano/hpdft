{-# LANGUAGE OverloadedStrings #-}

module Main where

import PDF.Definition

import PDF.Object
import PDF.PDFIO
import PDF.Outlines

import System.Environment (getArgs)

import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (nub)

import Debug.Trace

initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=0
                , leftmargin=0.0
                , top=0.0
                , bottom=0.0
                , fontfactor=1
                , curfont=""
                , fontmaps=[]
                , cmaps=[]
                , colorspace=""
                , xcolorspaces=[]
                }

main = do
  fn:_ <- getArgs
  pdfToText fn

-- | Get a whole text from 'filename'. It works as:
--    (1) grub objects
--    (2) parse within each object, deflating its stream
--    (3) linearize stream

pdfToText filename = do
  contents <- BS.readFile filename
  let objs = map parsePDFObj $ getObjs contents
  let rootref = case rootRef contents of
                  Just r  -> r
                  Nothing -> 0
  BSL.putStrLn $ linearize rootref objs

linearize :: Int -> [PDFObj] -> PDFStream
linearize parent objs = 
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case pages dict of 
        Just pr -> linearize pr objs
        Nothing -> ""
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case pagesKids dict of
          Just kidsrefs -> BSL.concat $ map ((\f -> f objs) . linearize) kidsrefs
          Nothing -> ""
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> contentsStream dict initstate objs
          Nothing -> ""
    Nothing -> ""
