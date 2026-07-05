{-# LANGUAGE OverloadedStrings #-}

module PDF.Text
  ( initstate
  , walkdown
  , pdfToTextBS
  , pdfToTextWithWarnings
  , pdfToTextDoc
  , pdfToTextGeomBS
  , pdfToTextGeomDoc
  ) where

import PDF.Definition
import PDF.Error (PdfResult, PdfWarning(..))
import PDF.Document (Document(..), openDocument, docRootRef)
import PDF.DocumentStructure
import PDF.Encrypt (Security)
import PDF.Interpret (interpretPageItems)
import PDF.Layout (layoutPageText)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.Text as T

initstate :: PSR
initstate = PSR { linex=0
                , liney=0
                , absolutex=0
                , absolutey=0
                , leftmargin=0.0
                , text_lm=(1,0,0,1,0,0)
                , text_m=(1,0,0,1,0,0)
                , text_break=False
                , top=0.0
                , bottom=0.0
                , fontfactor=1
                , curfont=""
                , fontmaps=Map.empty
                , cmaps=Map.empty
                , colorspace=""
                , xcolorspaces=[]
                , warnings=[]
                }

pdfToTextBS :: FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextBS filename mpw = do
  result <- pdfToTextWithWarnings filename mpw
  return (fmap fst result)

pdfToTextWithWarnings :: FilePath -> Maybe String -> IO (PdfResult (BSL.ByteString, [PdfWarning]))
pdfToTextWithWarnings filename mpw = do
  docResult <- openDocument filename mpw
  return $ do
    doc <- docResult
    rootref <- docRootRef doc
    return (walkdown initstate rootref (docSecurity doc) (docObjs doc))

pdfToTextDoc :: Document -> (BSL.ByteString, [PdfWarning])
pdfToTextDoc doc =
  case docRootRef doc of
    Right rootref -> walkdown initstate rootref (docSecurity doc) (docObjs doc)
    Left _ -> ("", [])

pdfToTextGeomBS :: FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextGeomBS filename mpw = do
  docResult <- openDocument filename mpw
  return (docResult >>= pdfToTextGeomDoc)

pdfToTextGeomDoc :: Document -> PdfResult BSL.ByteString
pdfToTextGeomDoc doc = do
  rootref <- docRootRef doc
  let refs = pageRefsOrder rootref (docObjs doc)
  pages <- mapM (interpretPageItems doc) refs
  return $ BSLU.fromString (T.unpack (T.concat (map layoutPageText pages)))

pageRefsOrder :: Int -> PDFObjIndex -> [Int]
pageRefsOrder parent objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> pageRefsOrder pr objs
        Nothing -> []
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kidsrefs -> concatMap (\k -> pageRefsOrder k objs) kidsrefs
          Nothing -> []
        Nothing -> case findDictOfType "/Page" os of
          Just _ -> [parent]
          Nothing -> []
    Nothing -> []

walkdown :: PSR -> Int -> Maybe Security -> PDFObjIndex -> (BSL.ByteString, [PdfWarning])
walkdown st parent sec objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> walkdown st pr sec objs
        Nothing -> ("", [])
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kidsrefs ->
            let results = map (\k -> walkdown st k sec objs) kidsrefs
            in ( BSL.concat (map fst results)
               , concatMap snd results
               )
          Nothing -> ("", [])
        Nothing -> case findDictOfType "/Page" os of
          Just dict -> pageContent dict st sec objs
          Nothing -> ("", [])
    Nothing -> ("", [])

pageContent :: Dict -> PSR -> Maybe Security -> PDFObjIndex -> (BSL.ByteString, [PdfWarning])
pageContent dict st sec objs =
  case contentsStream dict st sec objs of
    Right (s, ws) -> (s, ws)
    Left _ -> ("", [])
