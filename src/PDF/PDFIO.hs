module PDF.PDFIO ( getObjectByRef
                 , getPDFBSFromFile
                 , getPDFObjFromFile
                 , getRootRef
                 , getRootObj
                 , getTrailer
                 , getInfo
                 ) where

import PDF.Definition
import PDF.Object

import qualified Data.ByteString.Char8 as BS


-- IO utilities

getPDFBSFromFile :: String -> IO [PDFBS]
getPDFBSFromFile f = do
  c <- BS.readFile f
  let bs = getObjs c
  return bs

getPDFObjFromFile :: String -> IO [PDFObj]
getPDFObjFromFile f = do
  c <- BS.readFile f
  let obj = expandObjStm $ map parsePDFObj $ getObjs c
  return obj

getObjectByRef :: Int -> IO [PDFObj] -> IO [Obj]
getObjectByRef ref pdfobjs = do
  objs <- pdfobjs
  case findObjsByRef ref objs of
    Just os -> return $ os
    Nothing -> error $ "No Object with Ref " ++ show ref

getRootRefFromFile :: String -> IO (Maybe Int)
getRootRefFromFile f = do
  c <- BS.readFile f
  return $ rootRef c

getRootRef filename = do
  c <- BS.readFile filename
  let n = rootRef c
  case n of
    Just i -> return i
    Nothing -> error "Could not find rood object"
    
getRootObj filename = do
  rootref <- getRootRef filename
  objs <- getPDFObjFromFile filename
  case findObjsByRef rootref objs of
    Just os -> return os
    Nothing -> error "Could not get root object"

getTrailer filename = do
  c <- BS.readFile filename
  return $ findTrailer c

getInfo filename = do
  d <- getTrailer filename
  objs <- getPDFObjFromFile filename
  let inforef = case findObjThroughDict d "/Info" of
                  Just (ObjRef ref) -> ref
                  Just _ -> error "There seems to be no Info"
                  Nothing -> error "There seems to be no Info"
  case findDictByRef inforef objs of
    Just os -> return os
    Nothing -> error "Could not get info object"

getTitle filename = do
  d <- getInfo filename
  case findObjThroughDict d "/Title" of
    Just (PdfText s) -> return s
    Just x -> error $ show x
    Nothing -> error "No title anyway"

