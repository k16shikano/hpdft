{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Outlines
Description : Function to get /Outlines object
Copyright   : (c) Keiichiro Shikano, 2016
License     : MIT
Maintainer  : k16.shikano@gmail.com

Function to grub /Outlines in PDF trailer. It mainly provides texts for Table of Contents.
-}

module PDF.Outlines
       ( getOutlines
       ) where

import Debug.Trace

import Data.List (find)
import Data.Attoparsec.ByteString hiding (inClass, notInClass, satisfy)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS

import PDF.Definition hiding (toString)
import PDF.Object
import PDF.PDFIO

data PDFOutlines = PDFOutlinesTree [PDFOutlines]
                 | PDFOutlinesEntry { dest :: Int
                                    , text :: String
                                    , subs :: PDFOutlines
                                    }
                 | PDFOutlinesNE

instance Show PDFOutlines where
  show = toString 0

toString :: Int -> PDFOutlines -> String
toString depth PDFOutlinesEntry {dest=d, text=t, subs=s} = (replicate depth ' ' ++ t) ++ toString (depth+1) s
toString depth (PDFOutlinesTree os) = concatMap (toString depth) os
toString depth PDFOutlinesNE = ""

-- | Get information of \/Outlines from 'filename'

getOutlines :: FilePath -> IO PDFOutlines
getOutlines filename = do
  dict <- outlineObjFromFile filename
  objs <- getPDFObjFromFile filename  
  firstref <- case findFirst dict of
    Just r -> return r
    Nothing -> error "No top level outline entry."
  firstdict <- case findObjsByRef firstref objs of
    Just [PdfDict d] -> return d
    Nothing -> error $ "No Object with Ref " ++ show firstref
  return $ gatherOutlines firstdict objs

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
                                                            , text = findTitle dict objs ++ "\n"
                                                            , subs = c}
                                           : [gatherOutlines d objs])
      Nothing -> error $ "No Object with Ref " ++ show r
    Nothing -> PDFOutlinesEntry { dest = head $ findDest dict
                                , text = findTitle dict objs ++ "\n"
                                , subs = PDFOutlinesNE}

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
    Just (PdfText s) -> case parseOnly parsePdfLetters (BS.pack s) of
      Right t -> t
      Left err -> s
    Just (ObjRef r) -> case findObjsByRef r objs of
      Just [PdfText s] -> s
      Nothing -> error $ "No title object in " ++ show r
    Just x -> show x
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
