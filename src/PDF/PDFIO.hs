{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.PDFIO
Description : IO utilities for hpdft
Copyright   : (c) Keiichiro Shikano, 2016
License     : MIT
Maintainer  : k16.shikano@gmail.com

Functions for use within IO. 
-}

module PDF.PDFIO ( getObjectByRef
                 , getPDFBSFromFile
                 , getPDFObjFromFile
                 , getRootRef
                 , getRootObj
                 , getStream
                 , getTrailer
                 , getInfo
                 ) where

import PDF.Definition
import PDF.Object (parsePDFObj)
import PDF.DocumentStructure
  (rawStream, rawStreamByRef, findObjs', findObjsByRef, indexPDFObjs,
   findDict, findDictByRef, findObjFromDict, rootRef,
   findTrailer, expandObjStm)
import PDF.Encrypt (Security, securityFromEncryptDict)

import Data.List (find)
import Data.Maybe (fromMaybe)

import Debug.Trace

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BSL

loadSecurity :: BS.ByteString -> Maybe String -> Maybe (Maybe Security)
loadSecurity c password =
  case findTrailer c of
    Nothing -> Just Nothing
    Just trailer ->
      case findObjFromDict trailer "/Encrypt" of
        Nothing -> Just Nothing
        Just (ObjRef ref) ->
          let pw = fromMaybe "" password
              encDict = findEncryptDict c ref
          in case encDict of
               Just d ->
                 case securityFromEncryptDict d trailer (Just pw) of
                   Just sec -> Just (Just sec)
                   Nothing  -> Nothing
               _ -> Nothing
        _ -> Nothing

findEncryptDict :: BS.ByteString -> Int -> Maybe Dict
findEncryptDict c ref =
  case find ((== ref) . fst) (findObjs' c) of
    Nothing -> Nothing
    Just objbs -> findDict $ snd $ parsePDFObj Nothing objbs

-- | Get PDF objects as a whole bytestring. Use `getPDFObjFromFile` instead if there's no reason to see a raw bytestring. 

getPDFBSFromFile :: FilePath -> IO [PDFBS]
getPDFBSFromFile f = do
  c <- BS.readFile f
  let bs = findObjs' c
  return bs

-- | Get PDF objects each parsed as 'PDFObj' without being sorted. 

getPDFObjFromFile :: FilePath -> Maybe String -> IO (PDFObjIndex, Maybe Security)
getPDFObjFromFile f password = do
  c <- BS.readFile f
  case loadSecurity c password of
    Nothing -> error "Encrypted PDF: invalid or missing password"
    Just msec ->
      let objs = indexPDFObjs $ expandObjStm msec $ map (parsePDFObj msec) $ findObjs' c
      in return (objs, msec)

-- | Get a PDF object from a whole 'PDFObj' by specifying `ref :: Int`

getObjectByRef :: Int -> PDFObjIndex -> IO [Obj]
getObjectByRef ref pdfobjs = do
  case findObjsByRef ref pdfobjs of
    Just os -> return os
    Nothing -> error $ "No Object with Ref " ++ show ref

-- | Get a PDF stream from a whole 'PDFObj' by specifying `ref :: Int`

getStream :: Maybe Security -> Int -> Bool -> [Obj] -> IO BSL.ByteString
getStream sec objNum hex obj = return $ showBSL hex $ rawStream sec objNum obj

showBSL hex s =
  let strm' = (B.toLazyByteString . B.lazyByteStringHex) s
  in if hex
     then if BSL.length strm' > 256 then BSL.concat [BSL.take 256 strm', "...(omit)"] else strm'
     else s

-- | The reference number of /Root in `filename`.

getRootRef :: FilePath -> IO Int
getRootRef filename = do
  c <- BS.readFile filename
  let n = rootRef c
  case n of
    Just i -> return i
    Nothing -> error "Could not find rood object"
    
-- | The /Root object in `filename`.

getRootObj :: FilePath -> Maybe String -> IO [Obj]
getRootObj filename password = do
  rootref <- getRootRef filename
  (objs, _) <- getPDFObjFromFile filename password
  case findObjsByRef rootref objs of
    Just os -> return os
    Nothing -> error "Could not get root object"

-- | The trailer of `filename`.

getTrailer :: FilePath -> IO Dict
getTrailer filename = do
  c <- BS.readFile filename
  case findTrailer c of
    Just d -> return d
    Nothing -> return []

-- | /Info of `filename`.

getInfo :: FilePath -> Maybe String -> IO Dict
getInfo filename password = do
  c <- BS.readFile filename
  msec <- case loadSecurity c password of
    Nothing -> error "Encrypted PDF: invalid or missing password"
    Just s -> return s
  trailer <- case findTrailer c of
    Just d -> return d
    Nothing -> error "Could not find trailer"
  let inforef = case findObjFromDict trailer "/Info" of
                  Just (ObjRef ref) -> ref
                  Just _ -> error "There seems to be no Info"
                  Nothing -> error "There seems to be no Info"
  case find ((== inforef) . fst) (findObjs' c) of
    Nothing -> error "Could not get info object"
    Just objbs -> case findDict $ snd $ parsePDFObj msec objbs of
      Just d -> return d
      Nothing -> error "Could not get info object"
