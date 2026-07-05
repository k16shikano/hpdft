{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Text
Description : Text extraction drivers for hpdft
License     : MIT

High-level text extraction from an opened 'PDF.Document.Document' or a file path.

* __Default (CLI equivalent)__: 'pdfToTextTaggedDoc' tries tagged PDF structure
  extraction and falls back to geometry layout when structure is missing or unusable.
* __Geometry__: 'pdfToTextGeomDoc' reconstructs paragraphs from glyph positions
  (same pipeline as @hpdft text --geom@).
* __Tagged__: 'pdfToTextTaggedDocWith' forces the tagged path when a structure tree exists.
* __Legacy__: 'pdfToTextDoc' / 'walkdown' preserve the pre-0.3 stream-order extractor
  (same as @hpdft text --legacy@).

Pass 'PDF.Layout.LayoutOptions' to the @…With@ variants for footnotes and ruby.

@example
import PDF.Document (openDocument)
import PDF.Text (pdfToTextTaggedDoc)
import qualified Data.ByteString.Lazy.Char8 as BSL

main = do
  result <- openDocument "doc.pdf" Nothing
  case result of
    Left err -> print err
    Right doc ->
      case pdfToTextTaggedDoc doc of
        Left err -> print err
        Right bs -> BSL.putStr bs
-}
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
  , pdfToTextStreamDoc
  ) where

import PDF.Definition
import PDF.Error (PdfResult, PdfWarning(..))
import PDF.Document (Document(..), openDocument, docRootRef)
import PDF.DocumentStructure
import PDF.Encrypt (Security)
import PDF.Interpret (Glyph(..), Rect(..), PageItem(..), interpretPageItems)
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions, aozoraRuby, joinGlyphsRun, layoutDocumentFromPageLines, layoutPageTextWith, linesFromGlyphs, pageLinesRaw, stripHeadersFooters, joinParaLines, forcePageLines)
import PDF.Page (pageRefs)
import PDF.Structure (StructElem(..), RubySpan(..), structTree, logicalOrder, collectRubySpans)

import Control.DeepSeq (force)
import Control.Parallel.Strategies (Eval, parList, using)
import Data.List (foldl')
import qualified Data.Set as S
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

-- | Legacy stream-order extraction, one page at a time in document order.
--
-- The callback receives a 1-based page number, total page count, and that
-- page's legacy text (same bytes as the corresponding segment of 'pdfToTextDoc').
-- Concatenating all page outputs in order yields the 'fst' of 'pdfToTextDoc'.
-- Warnings from every page are collected and returned.
pdfToTextStreamDoc ::
  Document -> (Int -> Int -> BSL.ByteString -> IO ()) -> IO [PdfWarning]
pdfToTextStreamDoc doc callback =
  case pageRefs doc of
    Left _ -> return []
    Right refs -> do
      let total = length refs
          sec = docSecurity doc
          objs = docObjs doc
      wss <- mapM (streamPage total sec objs) (zip [1 ..] refs)
      return (concat wss)
  where
    streamPage total sec objs (num, ref) =
      case findObjsByRef ref objs of
        Just os ->
          case findDictOfType "/Page" os of
            Just dict -> do
              let (txt, ws) = pageContent dict initstate sec objs
              callback num total txt
              return ws
            Nothing -> return []
        Nothing -> return []

pdfToTextGeomBS :: FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextGeomBS = pdfToTextGeomBSWith defaultLayoutOptions

pdfToTextGeomBSWith :: LayoutOptions -> FilePath -> Maybe String -> IO (PdfResult BSL.ByteString)
pdfToTextGeomBSWith opts filename mpw = do
  docResult <- openDocument filename mpw
  return (docResult >>= pdfToTextGeomDocWith opts)

pdfToTextGeomDoc :: Document -> PdfResult BSL.ByteString
pdfToTextGeomDoc = pdfToTextGeomDocWith defaultLayoutOptions

-- | Geometry-based full-document text (paragraph layout across all pages).
pdfToTextGeomDocWith :: LayoutOptions -> Document -> PdfResult BSL.ByteString
pdfToTextGeomDocWith opts doc = do
  refs <- pageRefs doc
  layouts <- parallelPageResults (interpretPageLinesRaw doc) forcePageLines refs
  return (layoutPagesFromLines opts layouts)

interpretPageLinesRaw doc ref =
  pageLinesRaw <$> interpretPageItems doc ref

interpretAllPages :: Document -> PdfResult ([Int], [[PageItem]])
interpretAllPages doc = do
  refs <- pageRefs doc
  pages <- parallelPageResults (interpretPageItems doc) forcePageItems refs
  return (refs, pages)

-- | Evaluate per-page 'PdfResult' work in parallel, then sequence in page order.
parallelPageResults ::
  (Int -> PdfResult a) -> (a -> ()) -> [Int] -> PdfResult [a]
parallelPageResults f forcePayload refs =
  let results = map f refs `using` parList (evalResultPayload forcePayload)
  in sequence results

evalResultPayload :: (a -> ()) -> PdfResult a -> Eval (PdfResult a)
evalResultPayload forceIt result =
  case result of
    Left e -> e `seq` return result
    Right v -> forceIt v `seq` return result

-- | Force interpreted page items for parallel evaluation. 'FontInfo' width
-- functions inside the interpreter are not touched; only glyph text and coords.
forcePageItems :: [PageItem] -> ()
forcePageItems = foldr forcePageItem ()

forcePageItem :: PageItem -> () -> ()
forcePageItem (ItemGlyph g) () = forceGlyph g
forcePageItem (ItemGraphic r) () =
  rectX0 r `seq` rectY0 r `seq` rectX1 r `seq` rectY1 r `seq` ()

forceGlyph :: Glyph -> ()
forceGlyph g =
  force (glyphText g) `seq`
  glyphX g `seq` glyphY g `seq` glyphWidth g `seq` glyphSize g `seq`
  force (glyphFont g) `seq` glyphWMode g `seq` glyphMCID g `seq` ()

layoutPagesFromLines opts layouts =
  BSLU.fromString (T.unpack (layoutDocumentFromPageLines opts layouts))

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

-- | Tagged structure extraction with geometry fallback (default CLI behaviour).
pdfToTextTaggedDocWith :: LayoutOptions -> Document -> PdfResult BSL.ByteString
pdfToTextTaggedDocWith opts doc = do
  mroot <- structTree doc
  case mroot of
    Nothing -> pdfToTextGeomDocWith opts doc
    Just root -> do
      (refs, pages) <- interpretAllPages doc
      if taggedUsable pages
         then Right (BSLU.fromString (T.unpack (assembleTagged opts root refs pages)))
         else Right (layoutPagesFromLines opts (map pageLinesRaw pages))

taggedUsable :: [[PageItem]] -> Bool
taggedUsable pages =
  let glyphs = [g | pg <- pages, ItemGlyph g <- pg]
      total = length glyphs
      tagged = length [g | g <- glyphs, glyphMCID g /= Nothing]
  in total > 0 && fromIntegral tagged / fromIntegral total >= 0.5

assembleTagged :: LayoutOptions -> StructElem -> [Int] -> [[PageItem]] -> T.Text
assembleTagged opts root refs pages =
  let mcidMaps = zip refs (map mcidGlyphMap pages)
      mcidLookup = Map.fromList
        [((page, mcid), gs) | (page, m) <- mcidMaps, (mcid, gs) <- Map.toList m]
      rubyMap = structureRubyMap opts root refs pages
      structRubyPages = S.fromList [rsPage s | s <- collectRubySpans root]
      geomRubyPerPage =
        Map.fromList
          [ (page, layoutPageTextWith opts items)
          | (page, items) <- zip refs pages
          , optRuby opts
          , not (page `S.member` structRubyPages)
          ]
      artifactLinesPerPage =
        Map.fromList $
          zip refs
            (stripHeadersFooters (length pages)
               (map (linesFromGlyphs . artifactGlyphs) pages))
      geomRubyEmitted = Map.empty :: Map.Map Int Bool

      lastPathType [] = T.empty
      lastPathType path = last path

      snocSep hasContent prevEnd acc =
        if prevEnd && hasContent
        then acc . (T.append "\n\n")
        else acc

      appendMCID (acc, hasContent, prevParaEnd, emitted) (path, page, mcid) =
        if Map.member page geomRubyPerPage && not (Map.findWithDefault False page emitted)
        then let run = Map.findWithDefault T.empty page geomRubyPerPage
             in ( snocSep hasContent prevParaEnd acc . T.append run
                , True
                , False
                , Map.insert page True emitted
                )
        else case Map.lookup (page, mcid) mcidLookup of
          Nothing -> (acc, hasContent, prevParaEnd, emitted)
          Just gs ->
            let run = joinGlyphsRun gs
                paraEnd = paragraphEnd (lastPathType path)
                formatted = case Map.lookup (page, mcid) rubyMap of
                              Just rt -> aozoraRuby run rt
                              _       -> run
            in ( snocSep hasContent prevParaEnd acc . T.append formatted
               , True
               , paraEnd
               , emitted
               )

      appendArtifacts (acc, hasContent, emitted) page =
        if Map.member page geomRubyPerPage
        then (acc, hasContent, emitted)
        else case Map.lookup page artifactLinesPerPage of
               Just ls | not (null ls) ->
                 let run = joinParaLines ls
                 in if T.null run
                    then (acc, hasContent, emitted)
                    else if not hasContent
                         then (acc . T.append run, True, emitted)
                         else (acc . (T.append "\n\n") . T.append run, True, emitted)
               _ -> (acc, hasContent, emitted)

      order = logicalOrder root
      (structAcc, structHasContent, _, _) =
        foldl' appendMCID (id, False, False, geomRubyEmitted) order
      (outAcc, _, _) =
        foldl' appendArtifacts (structAcc, structHasContent, geomRubyEmitted) refs
      out = outAcc T.empty
  in if T.null out then "\n" else out `T.append` "\n"

structureRubyMap ::
  LayoutOptions -> StructElem -> [Int] -> [[PageItem]] -> Map.Map (Int, Int) T.Text
structureRubyMap opts root refs pages
  | not (optRuby opts) = Map.empty
  | otherwise =
      let mcidMaps = Map.fromList (zip refs (map mcidGlyphMap pages))
          glyphTextFor page mcid =
            case Map.lookup page mcidMaps >>= Map.lookup mcid of
              Just gs -> joinGlyphsRun gs
              _       -> T.empty
      in Map.fromList
           [ ((rsPage span, baseMcid), rubyTxt)
           | span <- collectRubySpans root
           , (baseMcid, rubyMcid) <- zip (rsBases span) (rsRubies span)
           , let rubyTxt = glyphTextFor (rsPage span) rubyMcid
           , not (T.null rubyTxt)
           ]

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
