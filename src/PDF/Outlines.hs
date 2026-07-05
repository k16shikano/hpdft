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

import qualified Data.Map as M
import Control.Monad (msum)
import Data.Attoparsec.ByteString hiding (inClass, notInClass, satisfy)
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Char8 as BS

import PDF.Definition hiding (toString)
import PDF.Document (Document(..), openDocument, docRootRef)
import PDF.DocumentStructure
import PDF.Error (PdfError(..), PdfResult)
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
        Right (dict, destsRoot) -> do
          let objs = docObjs doc
          case findFirst dict of
            Nothing -> return $ Left (MissingKey "/First" "outlines")
            Just firstref -> case findObjsByRef firstref objs of
              Just [PdfDict d] -> return $ gatherOutlines d objs destsRoot
              Just s -> return $ Left (ParseError ("Unknown outline object: " ++ show s) BS.empty)
              Nothing -> return $ Left (MissingObject firstref)

gatherChildren :: Dict -> PDFObjIndex -> Maybe Int -> PdfResult PDFOutlines
gatherChildren dict objs destsRoot = case findFirst dict of
  Just r -> case findObjsByRef r objs of
    Just [PdfDict d] -> gatherOutlines d objs destsRoot
    Just s -> Left (ParseError ("Unknown outline child: " ++ show s) BS.empty)
    Nothing -> Left (MissingObject r)
  Nothing -> Right PDFOutlinesNE

gatherOutlines :: Dict -> PDFObjIndex -> Maybe Int -> PdfResult PDFOutlines
gatherOutlines dict objs destsRoot = do
  c <- gatherChildren dict objs destsRoot
  let dest = destPage dict objs destsRoot
  title <- findTitle dict objs
  case findNext dict of
    Just r -> case findObjsByRef r objs of
      Just [PdfDict d] -> do
        next <- gatherOutlines d objs destsRoot
        return $ PDFOutlinesTree (PDFOutlinesEntry { dest = dest
                                                   , text = title ++ "\n"
                                                   , subs = c}
                                  : [next])
      Just s -> Left (ParseError ("Unknown outline sibling: " ++ show s) BS.empty)
      Nothing -> Left (MissingObject r)
    Nothing -> return $ PDFOutlinesEntry { dest = dest
                                         , text = title ++ "\n"
                                         , subs = c}

destPage :: Dict -> PDFObjIndex -> Maybe Int -> Int
destPage dict objs destsRoot =
  maybe 0 id $ listToMaybe $ findDest dict objs destsRoot

findDest :: Dict -> PDFObjIndex -> Maybe Int -> [Int]
findDest dict objs destsRoot =
  case findObjFromDict dict "/Dest" of
    Just o -> destFromObj o objs
    Nothing -> case findObjFromDict dict "/A" of
      Just (ObjRef r) -> actionDest r objs destsRoot
      Just (PdfDict d) -> destFromAction d objs destsRoot
      _ -> []

destFromObj :: Obj -> PDFObjIndex -> [Int]
destFromObj (PdfArray a) _ = parseRefsArray a
destFromObj (ObjRef r) objs =
  case findObjsByRef r objs of
    Just (o:_) -> destFromObj o objs
    _ -> []
destFromObj (PdfNumber n) _ | truncate n >= 0 = [truncate n]
destFromObj _ _ = []

actionDest :: Int -> PDFObjIndex -> Maybe Int -> [Int]
actionDest r objs destsRoot = case findObjsByRef r objs of
  Just (PdfDict d : _) -> destFromAction d objs destsRoot
  _ -> []

destFromAction :: Dict -> PDFObjIndex -> Maybe Int -> [Int]
destFromAction d objs destsRoot =
  case findObjFromDict d "/D" of
    Just o -> destFromGoTo o objs destsRoot
    Nothing -> []

destFromGoTo :: Obj -> PDFObjIndex -> Maybe Int -> [Int]
destFromGoTo o objs destsRoot =
  case objAsName o of
    Just name -> lookupNamedDest destsRoot name objs
    Nothing -> destFromObj o objs

lookupNamedDest :: Maybe Int -> T.Text -> PDFObjIndex -> [Int]
lookupNamedDest Nothing _ _ = []
lookupNamedDest (Just root) name objs =
  case lookupNameNode root name objs of
    Just o -> destFromNamedDest o objs
    Nothing -> []

lookupNameNode :: Int -> T.Text -> PDFObjIndex -> Maybe Obj
lookupNameNode ref name objs =
  resolveDict ref objs >>= \d ->
    case findObjFromDict d "/Names" of
      Just (PdfArray arr) -> lookupNamePair arr name
      _ -> case findObjFromDict d "/Kids" of
        Just (PdfArray kids) ->
          msum [ lookupNameNode r name objs
               | ObjRef r <- kids
               , nameInLimits name r objs
               ]
        _ -> Nothing

lookupNamePair :: [Obj] -> T.Text -> Maybe Obj
lookupNamePair (n : v : rest) name =
  case objAsName n of
    Just t | t == name -> Just v
    _ -> lookupNamePair rest name
lookupNamePair _ _ = Nothing

nameInLimits :: T.Text -> Int -> PDFObjIndex -> Bool
nameInLimits name ref objs =
  case resolveDict ref objs of
    Nothing -> True
    Just d -> case findObjFromDict d "/Limits" of
      Just (PdfArray [lo, hi]) ->
        case (objAsName lo, objAsName hi) of
          (Just a, Just b) -> a <= name && name <= b
          _ -> True
      _ -> True

destFromNamedDest :: Obj -> PDFObjIndex -> [Int]
destFromNamedDest o objs =
  case o of
    ObjRef r -> case findObjsByRef r objs of
      Just (destObj:_) ->
        case destObj of
          PdfDict d -> maybe (destFromObj destObj objs) (\dd -> destFromObj dd objs)
                         $ findObjFromDict d "/D"
          _ -> destFromObj destObj objs
      _ -> []
    PdfDict d -> maybe [] (`destFromObj` objs) $ findObjFromDict d "/D"
    _ -> destFromObj o objs

resolveDict :: Int -> PDFObjIndex -> Maybe Dict
resolveDict r objs = case findObjsByRef r objs of
  Just (PdfDict d : _) -> Just d
  _ -> Nothing

objAsName :: Obj -> Maybe T.Text
objAsName (PdfName n) = Just n
objAsName (PdfText t) = Just t
objAsName _ = Nothing

outlinesRef :: Dict -> PdfResult Int
outlinesRef dict = case M.lookup "/Outlines" dict of
  Just (ObjRef x) -> Right x
  Just s -> Left (ParseError ("Unknown /Outlines: " ++ show s) BS.empty)
  Nothing -> Left (MissingKey "/Outlines" "root")

outlineFromDoc :: Document -> IO (PdfResult (Dict, Maybe Int))
outlineFromDoc doc =
  case docRootRef doc of
    Left err -> return (Left err)
    Right rootref -> outlineFromRoot rootref (docObjs doc)

outlineFromRoot :: Int -> PDFObjIndex -> IO (PdfResult (Dict, Maybe Int))
outlineFromRoot rootref objs =
  case findObjsByRef rootref objs of
    Nothing -> return $ Left (MissingObject rootref)
    Just rootobj -> case findDict rootobj of
      Nothing -> return $ Left (ParseError "root is not a dictionary" BS.empty)
      Just dict -> case outlinesRef dict of
        Left err -> return (Left err)
        Right outlineref -> case findObjsByRef outlineref objs of
          Just [PdfDict d] -> return (Right (d, destsRootRef dict objs))
          Just s -> return $ Left (ParseError ("Unknown outlines object: " ++ show s) BS.empty)
          Nothing -> return $ Left (MissingObject outlineref)

destsRootRef :: Dict -> PDFObjIndex -> Maybe Int
destsRootRef dict objs =
  resolveNamesDict dict objs >>= \names ->
    case findObjFromDict names "/Dests" of
      Just (ObjRef r) -> Just r
      _ -> Nothing

resolveNamesDict :: Dict -> PDFObjIndex -> Maybe Dict
resolveNamesDict dict objs =
  case findObjFromDict dict "/Names" of
    Just (PdfDict names) -> Just names
    Just (ObjRef r) -> resolveDict r objs
    _ -> Nothing

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
