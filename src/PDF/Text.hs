{-# LANGUAGE OverloadedStrings #-}

module PDF.Text
  ( initstate
  , walkdown
  , pdfToTextBS
  , pdfToTextWithWarnings
  , pdfToTextDoc
  , pdfToTextGeomBS
  , pdfToTextGeomBSWith
  , pdfToTextGeomDoc
  , pdfToTextGeomDocWith
  , pdfToTextTaggedBS
  , pdfToTextTaggedBSWith
  , pdfToTextTaggedDoc
  , pdfToTextTaggedDocWith
  , pageTextGeom
  , pageTextGeomWith
  ) where

import PDF.Definition
import PDF.Error (PdfResult, PdfWarning(..))
import PDF.Document (Document(..), openDocument, docRootRef)
import PDF.DocumentStructure
import PDF.Encrypt (Security)
import PDF.Interpret (Glyph(..), PageItem(..), interpretPageItems)
import PDF.Layout (LayoutOptions, defaultLayoutOptions, joinGlyphsRun, layoutDocumentWith, layoutPageTextWith, linesFromGlyphs, stripHeadersFooters, joinParaLines)
import PDF.Structure (StructElem(..), structTree, logicalOrder)

import Data.List (foldl')
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
                , curfont=T.empty
                , fontmaps=Map.empty
                , cmaps=Map.empty
                , colorspace=T.empty
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
pdfToTextGeomBS = pdfToTextGeomBSWith defaultLayoutOptions

pdfToTextGeomBSWith :: LayoutOptions -> FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextGeomBSWith opts filename mpw = do
  docResult <- openDocument filename mpw
  return (docResult >>= pdfToTextGeomDocWith opts)

pdfToTextGeomDoc :: Document -> PdfResult BSL.ByteString
pdfToTextGeomDoc = pdfToTextGeomDocWith defaultLayoutOptions

pdfToTextGeomDocWith :: LayoutOptions -> Document -> PdfResult BSL.ByteString
pdfToTextGeomDocWith opts doc = do
  rootref <- docRootRef doc
  let refs = pageRefsOrder rootref (docObjs doc)
  pages <- mapM (interpretPageItems doc) refs
  return $ BSLU.fromString (T.unpack (layoutDocumentWith opts pages))

pageTextGeom :: Document -> Int -> PdfResult BSL.ByteString
pageTextGeom = pageTextGeomWith defaultLayoutOptions

pageTextGeomWith :: LayoutOptions -> Document -> Int -> PdfResult BSL.ByteString
pageTextGeomWith opts doc pageRef = do
  items <- interpretPageItems doc pageRef
  return $ BSLU.fromString (T.unpack (layoutPageTextWith opts items))

pdfToTextTaggedBS :: FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextTaggedBS = pdfToTextTaggedBSWith defaultLayoutOptions

pdfToTextTaggedBSWith :: LayoutOptions -> FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextTaggedBSWith opts filename mpw = do
  docResult <- openDocument filename mpw
  return (docResult >>= pdfToTextTaggedDocWith opts)

pdfToTextTaggedDoc :: Document -> PdfResult BSL.ByteString
pdfToTextTaggedDoc = pdfToTextTaggedDocWith defaultLayoutOptions

pdfToTextTaggedDocWith :: LayoutOptions -> Document -> PdfResult BSL.ByteString
pdfToTextTaggedDocWith opts doc = do
  mroot <- structTree doc
  case mroot of
    Nothing -> pdfToTextGeomDocWith opts doc
    Just root -> do
      rootref <- docRootRef doc
      let refs = pageRefsOrder rootref (docObjs doc)
      pages <- mapM (interpretPageItems doc) refs
      if taggedUsable pages
         then Right (BSLU.fromString (T.unpack (assembleTagged root refs pages)))
         else pdfToTextGeomDocWith opts doc

taggedUsable :: [[PageItem]] -> Bool
taggedUsable pages =
  let glyphs = [g | pg <- pages, ItemGlyph g <- pg]
      total = length glyphs
      tagged = length [g | g <- glyphs, glyphMCID g /= Nothing]
  in total > 0 && fromIntegral tagged / fromIntegral total >= 0.5

assembleTagged :: StructElem -> [Int] -> [[PageItem]] -> T.Text
assembleTagged root refs pages =
  let mcidMaps = zip refs (map mcidGlyphMap pages)
      mcidLookup = Map.fromList
        [((page, mcid), gs) | (page, m) <- mcidMaps, (mcid, gs) <- Map.toList m]
      artifactLinesPerPage =
        Map.fromList $
          zip refs
            (stripHeadersFooters (length pages)
               (map (linesFromGlyphs . artifactGlyphs) pages))

      lastPathType [] = T.empty
      lastPathType path = last path

      appendMCID (acc, prevParaEnd) (path, page, mcid) =
        case Map.lookup (page, mcid) mcidLookup of
          Nothing -> (acc, prevParaEnd)
          Just gs ->
            let run = joinGlyphsRun gs
                sep = if prevParaEnd && not (T.null acc) then "\n\n" else ""
                paraEnd = paragraphEnd (lastPathType path)
            in (acc `T.append` sep `T.append` run, paraEnd)

      appendArtifacts acc page =
        case Map.lookup page artifactLinesPerPage of
          Just ls | not (null ls) ->
            let run = joinParaLines ls
            in if T.null acc
               then run
               else acc `T.append` "\n\n" `T.append` run
          _ -> acc

      order = logicalOrder root
      (structText, _) = foldl' appendMCID (T.empty, False) order
      withArtifacts = foldl' appendArtifacts structText refs
  in if T.null withArtifacts then "\n" else withArtifacts `T.append` "\n"

paragraphEnd :: T.Text -> Bool
paragraphEnd stype = stype `elem`
  [ "/P", "/H1", "/H2", "/H3", "/H4", "/H5", "/H6"
  , "/LI", "/LBody", "/TD", "/TH", "/Caption", "/Title"
  ]

mcidGlyphMap :: [PageItem] -> Map.Map Int [Glyph]
mcidGlyphMap items =
  Map.map reverse $
    Map.fromListWith (++) [(mcid, [g]) | ItemGlyph g <- items, Just mcid <- [glyphMCID g]]

artifactGlyphs :: [PageItem] -> [Glyph]
artifactGlyphs items = [g | ItemGlyph g <- items, glyphMCID g == Nothing]

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
