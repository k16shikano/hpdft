{-# LANGUAGE OverloadedStrings #-}

module PDF.Text
  ( initstate
  , walkdown
  , pdfToTextBS
  ) where

import PDF.Definition
import PDF.DocumentStructure
import PDF.PDFIO
import PDF.Encrypt (Security)

import qualified Data.ByteString.Lazy.Char8 as BSL

initstate :: PSR
initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=0
                , leftmargin=0.0
                , text_lm=(1,0,0,1,0,0)
                , text_m=(1,0,0,1,0,0)
                , text_break=False
                , top=0.0
                , bottom=0.0
                , fontfactor=1
                , curfont=""
                , fontmaps=[]
                , cmaps=[]
                , colorspace=""
                , xcolorspaces=[]
                }

pdfToTextBS :: FilePath -> Maybe String -> IO BSL.ByteString
pdfToTextBS filename mpw = do
  (objs, sec) <- getPDFObjFromFile filename mpw
  rootref <- getRootRef filename
  return $ walkdown initstate rootref sec objs

walkdown :: PSR -> Int -> Maybe Security -> PDFObjIndex -> PDFStream
walkdown st parent sec objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> walkdown st pr sec objs
        Nothing -> ""
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kidsrefs -> BSL.concat $ map ((\f -> f sec objs) . (walkdown st)) kidsrefs
          Nothing -> ""
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> contentsStream dict st sec objs
          Nothing -> ""
    Nothing -> ""
