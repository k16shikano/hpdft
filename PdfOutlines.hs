{-# LANGUAGE OverloadedStrings #-}

module PdfOutlines
       ( getOutlines
       ) where

import Debug.Trace

import Data.List (find)
import Pdf
import PdfObj

data PDFOutlinesTree = PDFOutlinesChildren [PDFOutlinesTree]
                     | PDFOutlinesEntry { dest :: Int
                                        , outlinetext :: String
                                        , subentries :: PDFOutlinesTree} 
                     | PDFOutlinesNE
                       deriving (Show)

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
  firstdict <- case findObjsByRef firstref objs of
    Just [PdfDict d] -> return $ d
    Nothing -> error $ "No Object with Ref " ++ show firstref
  return $ gatherOutlines firstdict objs

gatherOutlines dict objs =
  case findNext dict of 
    Just r -> case findObjsByRef r objs of
      Just [PdfDict d] -> PDFOutlinesEntry { dest = head $ findDest dict
                                           , outlinetext = findTitle dict
                                           , subentries = gatherOutlines d objs}
      Nothing -> error "?"
    Nothing -> PDFOutlinesEntry { dest = head $ findDest dict
                                , outlinetext = findTitle dict
                                , subentries = PDFOutlinesNE}


findTitle dict = 
  case findObjThroughDict dict "/Title" of
    Just (PdfText s) -> s
    Nothing -> error "No title object."

findDest dict = 
  case findObjThroughDict dict "/Dest" of
    Just (PdfArray a) -> parseRefsArray a
    Nothing -> error "No destination object."

findNext dict = 
  case findObjThroughDict dict "/Next" of
    Just (ObjRef x) -> Just x
    Nothing -> Nothing

findFirst dict =
  case findObjThroughDict dict "/First" of
    Just (ObjRef x) -> Just x
    Nothing -> Nothing
