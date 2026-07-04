{-# LANGUAGE OverloadedStrings #-}

module PDF.Document
  ( Document(..)
  , openDocument
  , docRootRef
  , docInfoDict
  ) where

import PDF.Definition
import PDF.DocumentStructure
  ( buildIndex
  , buildIndexEager
  , findTrailer
  , findTrailer'
  , findObjFromDict
  , findDict
  , findDictByRef
  , findRefs
  , extractObjBody
  , findObjs
  )
import PDF.Encrypt (Security, securityFromEncryptDict)
import PDF.Error (PdfError(..), PdfResult)
import PDF.Object (parsePDFObj)

import Data.List (find)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

data Document = Document
  { docTrailer  :: Dict
  , docObjs     :: PDFObjIndex
  , docSecurity :: Maybe Security
  }

openDocument :: FilePath -> Maybe String -> IO (PdfResult Document)
openDocument f password = do
  bytes <- BS.readFile f
  return (openDocumentBytes bytes password)

openDocumentBytes :: BS.ByteString -> Maybe String -> PdfResult Document
openDocumentBytes bytes password =
  case findTrailer' bytes of
    Right (trailer, xref) -> do
      msec <- loadSecurity bytes trailer xref password
      let objs = buildIndex bytes msec xref
      return (Document trailer objs msec)
    Left _ -> do
      trailer <- findTrailer bytes
      msec <- loadSecurityScan bytes trailer password
      objs <- buildIndexEager bytes msec
      return (Document trailer objs msec)

loadSecurity :: BS.ByteString -> Dict -> XREF -> Maybe String -> PdfResult (Maybe Security)
loadSecurity bytes trailer xref password =
  case findObjFromDict trailer "/Encrypt" of
    Nothing -> Right Nothing
    Just (ObjRef ref) ->
      case findEncryptDict bytes xref ref of
        Just d ->
          let pw = fromMaybe "" password
          in case securityFromEncryptDict d trailer (Just pw) of
               Just sec -> Right (Just sec)
               Nothing  -> Left (DecryptionError "invalid or missing password")
        Nothing -> Left (DecryptionError "invalid or missing password")
    _ -> Left (DecryptionError "invalid or missing password")

loadSecurityScan :: BS.ByteString -> Dict -> Maybe String -> PdfResult (Maybe Security)
loadSecurityScan bytes trailer password =
  case findObjFromDict trailer "/Encrypt" of
    Nothing -> Right Nothing
    Just (ObjRef ref) ->
      case findEncryptDictScan bytes ref of
        Just d ->
          let pw = fromMaybe "" password
          in case securityFromEncryptDict d trailer (Just pw) of
               Just sec -> Right (Just sec)
               Nothing  -> Left (DecryptionError "invalid or missing password")
        Nothing -> Left (DecryptionError "invalid or missing password")
    _ -> Left (DecryptionError "invalid or missing password")

findEncryptDict :: BS.ByteString -> XREF -> Int -> Maybe Dict
findEncryptDict bytes xref ref =
  case Map.lookup ref xref of
    Just (InFile off) ->
      let body = extractObjBody bytes off
      in findDict (snd (parsePDFObj Nothing (ref, body)))
    _ -> findEncryptDictScan bytes ref

findEncryptDictScan :: BS.ByteString -> Int -> Maybe Dict
findEncryptDictScan bytes ref =
  case find ((== ref) . fst) (findObjs bytes) of
    Just objbs -> findDict (snd (parsePDFObj Nothing objbs))
    Nothing -> Nothing

docRootRef :: Document -> PdfResult Int
docRootRef doc =
  case findRefs "/Root" (docTrailer doc) of
    Just r -> Right r
    Nothing -> Left (MissingKey "/Root" "trailer")

docInfoDict :: Document -> PdfResult Dict
docInfoDict doc =
  case findRefs "/Info" (docTrailer doc) of
    Nothing -> Left (MissingKey "/Info" "trailer")
    Just inforef ->
      case findDictByRef inforef (docObjs doc) of
        Just d -> Right d
        Nothing -> Left (MissingObject inforef)
