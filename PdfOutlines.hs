{-# LANGUAGE OverloadedStrings #-}

module PdfOutlines
       ( getOutlines
       ) where

import Debug.Trace

import Data.List (find)
import Pdf
import PdfObj

outlines :: Dict -> Int
outlines dict = case find isOutlinesRef dict of
  Just (_, ObjRef x) -> x
  Nothing            -> error "There seems no /Outlines in the root"
  where
    isOutlinesRef (PdfName "/Outlines", ObjRef x) = True
    isOutlinesRef (_,_)                           = False

outlineObjFromFile :: String -> IO Dict
outlineObjFromFile filename = do
  objs <- getPDFObjFromFile filename
  rootref <- getRootRef filename
  rootobj <- case findObjsByRef rootref objs of
    Just os -> return os
    Nothing -> error "Could not get root object."
  outlineref <- case findDict rootobj of
    Just dict -> return $ outlines dict
    Nothing   -> error "Something wrong..."
  case findObjsByRef outlineref objs of
    Just [PdfDict d] -> return d
    Nothing -> error "Could not get outlines object"
      
getOutlines filename = do
  dict <- outlineObjFromFile filename
  objs <- getPDFObjFromFile filename  
  firstref <- case findFirst dict of
    Just x -> return x
    Nothing -> error "No top level outline entry."
  return $ findNext firstref objs

findDest ref objs = 
  case findObjThroughDictByRef ref "/Dest" objs of
    Just (PdfArray a) -> parseRefsArray a
    Nothing -> error "No destination object."

findNext ref objs = 
  case findObjThroughDictByRef ref "/Next" objs of
    Just (ObjRef x) -> Just x
    Nothing -> Nothing

findFirst dict =
  case findObjThroughDict dict "/First" of
    Just (ObjRef x) -> Just x
    Nothing -> Nothing
