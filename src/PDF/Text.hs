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
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions, aozoraRuby, joinGlyphsRun, layoutDocumentWith, layoutPageTextWith, linesFromGlyphs, stripHeadersFooters, joinParaLines)
import PDF.Page (pageCount, pageRefAt)
import PDF.Structure (StructElem(..), RubySpan(..), structTree, logicalOrder, collectRubySpans)

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
  n <- pageCount doc
  refs <- mapM (pageRefAt doc) [1 .. n]
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
      n <- pageCount doc
      refs <- mapM (pageRefAt doc) [1 .. n]
      pages <- mapM (interpretPageItems doc) refs
      if taggedUsable pages
         then Right (BSLU.fromString (T.unpack (assembleTagged opts root refs pages)))
         else pdfToTextGeomDocWith opts doc

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

      appendMCID (acc, prevParaEnd, emitted) (path, page, mcid) =
        if Map.member page geomRubyPerPage && not (Map.findWithDefault False page emitted)
        then let run = Map.findWithDefault T.empty page geomRubyPerPage
                 sep = if prevParaEnd && not (T.null acc) then "\n\n" else ""
             in (acc `T.append` sep `T.append` run, False, Map.insert page True emitted)
        else case Map.lookup (page, mcid) mcidLookup of
          Nothing -> (acc, prevParaEnd, emitted)
          Just gs ->
            let run = joinGlyphsRun gs
                sep = if prevParaEnd && not (T.null acc) then "\n\n" else ""
                paraEnd = paragraphEnd (lastPathType path)
                formatted = case Map.lookup (page, mcid) rubyMap of
                              Just rt -> aozoraRuby run rt
                              _       -> run
            in (acc `T.append` sep `T.append` formatted, paraEnd, emitted)

      appendArtifacts (acc, emitted) page =
        if Map.member page geomRubyPerPage
        then (acc, emitted)
        else case Map.lookup page artifactLinesPerPage of
               Just ls | not (null ls) ->
                 let run = joinParaLines ls
                 in if T.null run
                    then (acc, emitted)
                    else if T.null acc
                         then (run, emitted)
                         else (acc `T.append` "\n\n" `T.append` run, emitted)
               _ -> (acc, emitted)

      order = logicalOrder root
      (structText, _, _) = foldl' appendMCID (T.empty, False, geomRubyEmitted) order
      (out, _) = foldl' appendArtifacts (structText, geomRubyEmitted) refs
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
