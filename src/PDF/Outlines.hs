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

import Data.List (find)
import qualified Data.Map as M
import Data.Attoparsec.ByteString hiding (inClass, notInClass, satisfy)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS

import PDF.Definition hiding (toString)
import PDF.Document (Document(..), openDocument, docRootRef)
import PDF.DocumentStructure
import PDF.Error (PdfError(..), PdfResult, note)
import PDF.Object (parseRefsArray, parsePdfLetters)

import qualified Data.Text as T

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

getOutlines :: FilePath -> Maybe String -> IO (PdfResult PDFOutlines)
getOutlines filename password = do
  docResult <- openDocument filename password
  case docResult of
    Left err -> return (Left err)
    Right doc -> do
      edict <- outlineFromDoc doc
      case edict of
        Left err -> return (Left err)
        Right dict -> do
          let objs = docObjs doc
          case findFirst dict of
            Nothing -> return $ Left (MissingKey "/First" "outlines")
            Just firstref -> case findObjsByRef firstref objs of
              Just [PdfDict d] -> return $ gatherOutlines d objs
              Just s -> return $ Left (ParseError ("Unknown outline object: " ++ show s) BS.empty)
              Nothing -> return $ Left (MissingObject firstref)

gatherChildren :: Dict -> PDFObjIndex -> PdfResult PDFOutlines
gatherChildren dict objs = case findFirst dict of
  Just r -> case findObjsByRef r objs of
    Just [PdfDict d] -> gatherOutlines d objs
    Just s -> Left (ParseError ("Unknown outline child: " ++ show s) BS.empty)
    Nothing -> Left (MissingObject r)
  Nothing -> Right PDFOutlinesNE

gatherOutlines :: Dict -> PDFObjIndex -> PdfResult PDFOutlines
gatherOutlines dict objs = do
  c <- gatherChildren dict objs
  dest <- destPage dict objs
  title <- findTitle dict objs
  case findNext dict of
    Just r -> case findObjsByRef r objs of
      Just [PdfDict d] -> do
        next <- gatherOutlines d objs
        return $ PDFOutlinesTree (PDFOutlinesEntry { dest = dest
                                                   , text = title ++ "\n"
                                                   , subs = c}
                                  : [next])
      Just s -> Left (ParseError ("Unknown outline sibling: " ++ show s) BS.empty)
      Nothing -> Left (MissingObject r)
    Nothing -> return $ PDFOutlinesEntry { dest = dest
                                         , text = title ++ "\n"
                                         , subs = c}

destPage :: Dict -> PDFObjIndex -> PdfResult Int
destPage dict objs =
  note (MissingKey "/Dest" "outline") $ listToMaybe $ findDest dict objs

findDest :: Dict -> PDFObjIndex -> [Int]
findDest dict objs =
  case findObjFromDict dict "/Dest" of
    Just o -> destFromObj o
    Nothing -> case findObjFromDict dict "/A" of
      Just (ObjRef r) -> actionDest r objs
      Just (PdfDict d) -> destFromAction d
      _ -> []

destFromObj :: Obj -> [Int]
destFromObj (PdfArray a) = parseRefsArray a
destFromObj (ObjRef r)   = [r]
destFromObj (PdfNumber n)| truncate n >= 0 = [truncate n]
destFromObj _            = []

actionDest :: Int -> PDFObjIndex -> [Int]
actionDest r objs = case findObjsByRef r objs of
  Just (PdfDict d : _) -> destFromAction d
  _ -> []

destFromAction :: Dict -> [Int]
destFromAction d = case findObjFromDict d "/D" of
  Just o -> destFromObj o
  Nothing -> []

outlinesRef :: Dict -> PdfResult Int
outlinesRef dict = case M.lookup "/Outlines" dict of
  Just (ObjRef x) -> Right x
  Just s -> Left (ParseError ("Unknown /Outlines: " ++ show s) BS.empty)
  Nothing -> Left (MissingKey "/Outlines" "root")

outlineFromDoc :: Document -> IO (PdfResult Dict)
outlineFromDoc doc =
  case docRootRef doc of
    Left err -> return (Left err)
    Right rootref -> outlineFromRoot rootref (docObjs doc)

outlineFromRoot :: Int -> PDFObjIndex -> IO (PdfResult Dict)
outlineFromRoot rootref objs =
  case findObjsByRef rootref objs of
    Nothing -> return $ Left (MissingObject rootref)
    Just rootobj -> case findDict rootobj of
      Nothing -> return $ Left (ParseError "root is not a dictionary" BS.empty)
      Just dict -> case outlinesRef dict of
        Left err -> return (Left err)
        Right outlineref -> case findObjsByRef outlineref objs of
          Just [PdfDict d] -> return (Right d)
          Just s -> return $ Left (ParseError ("Unknown outlines object: " ++ show s) BS.empty)
          Nothing -> return $ Left (MissingObject outlineref)

findTitle :: Dict -> PDFObjIndex -> PdfResult String
findTitle dict objs =
  case findObjFromDict dict "/Title" of
    Just (PdfText s) -> case parseOnly parsePdfLetters (BS.pack (T.unpack s)) of
      Right t -> Right (T.unpack t)
      Left _  -> Right (T.unpack s)
    Just (ObjRef r) -> case findObjsByRef r objs of
      Just [PdfText s] -> Right (T.unpack s)
      Just s -> Left (ParseError ("Unknown title object: " ++ show s) BS.empty)
      Nothing -> Left (MissingObject r)
    Just x -> Right (show x)
    Nothing -> Left (MissingKey "/Title" "outline")

listToMaybe :: [a] -> Maybe a
listToMaybe (x:_) = Just x
listToMaybe []    = Nothing

findNext :: Dict -> Maybe Int
findNext dict =
  case findObjFromDict dict "/Next" of
    Just (ObjRef x) -> Just x
    _ -> Nothing

findFirst :: Dict -> Maybe Int
findFirst dict =
  case findObjFromDict dict "/First" of
    Just (ObjRef x) -> Just x
    _ -> Nothing
