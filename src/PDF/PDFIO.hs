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
import PDF.DocumentStructure
  (rawStream, rawStreamByRef, findObjs, findObjs', findObjsByRef,
   findDictByRef, findObjFromDict, rootRef,
   findTrailer, expandObjStm)
import PDF.Object (parsePDFObj)

import Debug.Trace

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BSL

-- | Get PDF objects as a whole bytestring. Use `getPDFObjFromFile` instead if there's no reason to see a raw bytestring. 

getPDFBSFromFile :: FilePath -> IO [PDFBS]
getPDFBSFromFile f = do
  c <- BS.readFile f
  let bs = findObjs c
  return bs

-- | Get PDF objects each parsed as 'PDFObj' without being sorted. 

getPDFObjFromFile :: FilePath -> IO [PDFObj]
getPDFObjFromFile f = do
  c <- BS.readFile f
  let obj = expandObjStm $ map parsePDFObj $ findObjs' c
  return $ obj

-- | Get a PDF object from a whole 'PDFObj' by specifying `ref :: Int`

getObjectByRef :: Int -> [PDFObj] -> IO [Obj]
getObjectByRef ref pdfobjs = do
  case findObjsByRef ref pdfobjs of
    Just os -> return os
    Nothing -> error $ "No Object with Ref " ++ show ref

-- | Get a PDF stream from a whole 'PDFObj' by specifying `ref :: Int`

getStream :: Bool -> [Obj] -> IO BSL.ByteString
getStream hex obj = return $ showBSL hex $ rawStream obj

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

getRootObj :: FilePath -> IO [Obj]
getRootObj filename = do
  rootref <- getRootRef filename
  objs <- getPDFObjFromFile filename
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

getInfo :: FilePath -> IO Dict
getInfo filename = do
  d <- getTrailer filename
  objs <- getPDFObjFromFile filename
  let inforef = case findObjFromDict d "/Info" of
                  Just (ObjRef ref) -> ref
                  Just _ -> error "There seems to be no Info"
                  Nothing -> error "There seems to be no Info"
  case findDictByRef inforef objs of
    Just os -> return os
    Nothing -> error "Could not get info object"
