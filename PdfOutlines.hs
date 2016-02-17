{-# LANGUAGE OverloadedStrings #-}

module PdfOutlines
       ( getOutlines
       ) where

import Debug.Trace

import Data.List (find)
import Pdf
import PdfObj

data PDFOutlines = PDFOutlinesTree [PDFOutlines]
                 | PDFOutlinesEntry { dest :: Int
                                    , text :: String
                                    , subs :: PDFOutlines
                                    }
                 | PDFOutlinesNE
                 deriving (Show)

getOutlines filename = do
  dict <- outlineObjFromFile filename
  objs <- getPDFObjFromFile filename  
  firstref <- case findFirst dict of
    Just r -> return r
    Nothing -> error "No top level outline entry."
  firstdict <- case findObjsByRef firstref objs of
    Just [PdfDict d] -> return $ d
    Nothing -> error $ "No Object with Ref " ++ show firstref
  showOutlines 0 $ gatherOutlines firstdict objs

gatherChildren dict objs = case findFirst dict of
  Just r -> case findObjsByRef r objs of
    Just [PdfDict d] -> gatherOutlines d objs
    Nothing -> error $ "No Object with Ref " ++ show r
  Nothing -> PDFOutlinesNE

gatherOutlines dict objs =
  let c = gatherChildren dict objs
  in case findNext dict of 
    Just r -> case findObjsByRef r objs of
      Just [PdfDict d] -> PDFOutlinesTree (PDFOutlinesEntry { dest = head $ findDest dict
                                                            , text = findTitle dict objs
                                                            , subs = c}
                                           : [gatherOutlines d objs])
      Nothing -> error $ "No Object with Ref " ++ show r
    Nothing -> PDFOutlinesEntry { dest = head $ findDest dict
                                , text = findTitle dict objs
                                , subs = PDFOutlinesNE}

showOutlines :: Int -> PDFOutlines -> IO ()
showOutlines depth (PDFOutlinesEntry {dest=d, text=t, subs=s}) = putStrLn (replicate depth ' ' ++ t) >> showOutlines (depth+1) s
showOutlines depth (PDFOutlinesTree os) = mapM_ (showOutlines (depth)) os
showOutlines depth PDFOutlinesNE = putStr ""

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

findTitle dict objs = 
  case findObjThroughDict dict "/Title" of
    Just (PdfText s) -> s
    Just (ObjRef r) -> case findObjsByRef r objs of
      Just [PdfText s] -> s
      Nothing -> error $ "No title object in "++(show r)
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