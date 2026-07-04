{-# LANGUAGE OverloadedStrings #-}

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
import PDF.Document (Document(..), openDocument, docRootRef, docInfoDict)
import PDF.DocumentStructure (rawStream, findObjs', findObjsByRef)
import PDF.Encrypt (Security)
import PDF.Error (PdfError(..), PdfResult, note)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as BSL

getPDFBSFromFile :: FilePath -> IO (PdfResult [PDFBS])
getPDFBSFromFile f = do
  c <- BS.readFile f
  return (findObjs' c)

getPDFObjFromFile :: FilePath -> Maybe String -> IO (PdfResult (PDFObjIndex, Maybe Security))
getPDFObjFromFile f password = do
  docResult <- openDocument f password
  return (fmap docObjsSec docResult)
  where
    docObjsSec doc = (docObjs doc, docSecurity doc)

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
  docResult <- openDocument filename Nothing
  return (docResult >>= docRootRef)

getRootObj :: FilePath -> Maybe String -> IO (PdfResult [Obj])
getRootObj filename password = do
  docResult <- openDocument filename password
  return $ do
    doc <- docResult
    rootref <- docRootRef doc
    note (MissingObject rootref) (findObjsByRef rootref (docObjs doc))

getTrailer :: FilePath -> IO (PdfResult Dict)
getTrailer filename = do
  docResult <- openDocument filename Nothing
  return (fmap docTrailer docResult)

getInfo :: FilePath -> Maybe String -> IO (PdfResult Dict)
getInfo filename password = do
  docResult <- openDocument filename password
  return (docResult >>= docInfoDict)
