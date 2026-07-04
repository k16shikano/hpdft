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
  (rawStream, findObjs', findObjsByRef, indexPDFObjs,
   findDict, findDictByRef, findObjFromDict, rootRef,
   findTrailer, expandObjStm)
import PDF.Encrypt (Security, securityFromEncryptDict)
import PDF.Error (PdfError(..), PdfResult, note)

import Data.List (find)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BSL

loadSecurity :: BS.ByteString -> Maybe String -> PdfResult (Maybe Security)
loadSecurity c password =
  case findTrailer c of
    Left _ -> Right Nothing
    Right trailer ->
      case findObjFromDict trailer "/Encrypt" of
        Nothing -> Right Nothing
        Just (ObjRef ref) ->
          case findEncryptDict c ref of
            Just d ->
              let pw = fromMaybe "" password
              in case securityFromEncryptDict d trailer (Just pw) of
                   Just sec -> Right (Just sec)
                   Nothing  -> Left (DecryptionError "invalid or missing password")
            Nothing -> Left (DecryptionError "invalid or missing password")
        _ -> Left (DecryptionError "invalid or missing password")

findEncryptDict :: BS.ByteString -> Int -> Maybe Dict
findEncryptDict c ref =
  case findObjs' c of
    Left _ -> Nothing
    Right objs ->
      case find ((== ref) . fst) objs of
        Nothing -> Nothing
        Just objbs -> findDict $ snd $ parsePDFObj Nothing objbs

getPDFBSFromFile :: FilePath -> IO (PdfResult [PDFBS])
getPDFBSFromFile f = do
  c <- BS.readFile f
  return (findObjs' c)

getPDFObjFromFile :: FilePath -> Maybe String -> IO (PdfResult (PDFObjIndex, Maybe Security))
getPDFObjFromFile f password = do
  c <- BS.readFile f
  case loadSecurity c password of
    Left err -> return (Left err)
    Right msec ->
      case findObjs' c of
        Left err -> return (Left err)
        Right rawObjs ->
          let parsed = map (parsePDFObj msec) rawObjs
          in case expandObjStm msec parsed of
               Left err -> return (Left err)
               Right expanded ->
                 return (Right (indexPDFObjs expanded, msec))

getObjectByRef :: Int -> PDFObjIndex -> IO (PdfResult [Obj])
getObjectByRef ref pdfobjs =
  return (note (MissingObject ref) (findObjsByRef ref pdfobjs))

getStream :: Maybe Security -> Int -> Bool -> [Obj] -> IO (PdfResult BSL.ByteString)
getStream sec objNum hex obj =
  return (fmap (showBSL hex) (rawStream sec objNum obj))

showBSL :: Bool -> BSL.ByteString -> BSL.ByteString
showBSL hex s =
  let strm' = (B.toLazyByteString . B.lazyByteStringHex) s
  in if hex
     then if BSL.length strm' > 256 then BSL.concat [BSL.take 256 strm', "...(omit)"] else strm'
     else s

getRootRef :: FilePath -> IO (PdfResult Int)
getRootRef filename = do
  c <- BS.readFile filename
  case rootRef c of
    Left err -> return (Left err)
    Right (Just i) -> return (Right i)
    Right Nothing -> return (Left (MissingKey "/Root" "trailer"))

getRootObj :: FilePath -> Maybe String -> IO (PdfResult [Obj])
getRootObj filename password = do
  rootResult <- getRootRef filename
  case rootResult of
    Left err -> return (Left err)
    Right rootref -> do
      objResult <- getPDFObjFromFile filename password
      case objResult of
        Left err -> return (Left err)
        Right (objs, _) ->
          return (note (MissingObject rootref) (findObjsByRef rootref objs))

getTrailer :: FilePath -> IO (PdfResult Dict)
getTrailer filename = do
  c <- BS.readFile filename
  return (findTrailer c)

getInfo :: FilePath -> Maybe String -> IO (PdfResult Dict)
getInfo filename password = do
  c <- BS.readFile filename
  case loadSecurity c password of
    Left err -> return (Left err)
    Right msec ->
      case findTrailer c of
        Left err -> return (Left err)
        Right trailer ->
          case findObjFromDict trailer "/Info" of
            Nothing -> return (Left (MissingKey "/Info" "trailer"))
            Just (ObjRef inforef) -> lookupInfo c msec inforef
            Just _ -> return (Left (MissingKey "/Info" "trailer"))

lookupInfo :: BS.ByteString -> Maybe Security -> Int -> IO (PdfResult Dict)
lookupInfo c msec inforef =
  case findObjs' c of
    Left err -> return (Left err)
    Right rawObjs ->
      case find ((== inforef) . fst) rawObjs of
        Just objbs ->
          case findDict (snd (parsePDFObj msec objbs)) of
            Just d -> return (Right d)
            Nothing -> getInfoFromIndex c msec inforef
        Nothing -> getInfoFromIndex c msec inforef

getInfoFromIndex :: BS.ByteString -> Maybe Security -> Int -> IO (PdfResult Dict)
getInfoFromIndex c msec inforef =
  case findObjs' c of
    Left err -> return (Left err)
    Right rawObjs ->
      let parsed = map (parsePDFObj msec) rawObjs
      in case expandObjStm msec parsed of
           Left err -> return (Left err)
           Right expanded ->
             case findDictByRef inforef (indexPDFObjs expanded) of
               Just d -> return (Right d)
               Nothing -> return (Left (MissingObject inforef))
