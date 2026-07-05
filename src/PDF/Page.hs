{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Page
Description : Public page-level API for hpdft (0.4)
License     : MIT

Stable entry points for page enumeration and structured extraction.
Scripts and downstream tools should prefer this module over
'PDF.DocumentStructure' internals.
-}
module PDF.Page
  ( PageRef
  , pageCount
  , pageRefAt
  , pageItems
  , pageGlyphs
  , pageLines
  , pageParagraphs
  , PageRegion(..)
  , pageRegions
  ) where

import PDF.Definition (PDFObjIndex)
import PDF.Document (Document(..), docRootRef)
import PDF.DocumentStructure
  ( findDictOfType
  , findKids
  , findObjsByRef
  , findPages
  )
import PDF.Error (PdfError(..), PdfResult)
import PDF.Interpret (Glyph(..), PageItem(..), Rect(..), interpretPageItems)
import PDF.Layout
  ( LayoutOptions
  , Line(..)
  , layoutParagraphsWith
  , pageItemLines
  , pageItemParagraphGroups
  )

import Data.List (elemIndex)

import qualified Data.Text as T

-- | PDF page object reference number (same as catalog\/pages leaf ref).
type PageRef = Int

-- | Number of pages in document order (1-based page numbers run @1 .. pageCount@).
pageCount :: Document -> PdfResult Int
pageCount doc = fmap length (pageRefs doc)

-- | Resolve a 1-based page number to its object reference.
pageRefAt :: Document -> Int -> PdfResult PageRef
pageRefAt doc n
  | n < 1 = Left (UnsupportedFeature ("page number " ++ show n ++ " out of range"))
  | otherwise = do
      refs <- pageRefs doc
      if n > length refs
        then Left (UnsupportedFeature ("page number " ++ show n ++ " out of range (1-" ++ show (length refs) ++ ")"))
        else Right (refs !! (n - 1))

pageItems :: Document -> PageRef -> PdfResult [PageItem]
pageItems = interpretPageItems

pageGlyphs :: Document -> PageRef -> PdfResult [Glyph]
pageGlyphs doc ref = fmap extractGlyphs (pageItems doc ref)
  where
    extractGlyphs items = [g | ItemGlyph g <- items]

pageLines :: Document -> PageRef -> LayoutOptions -> PdfResult [Line]
pageLines doc ref opts = fmap (pageItemLines opts) (pageItems doc ref)

pageParagraphs :: Document -> PageRef -> LayoutOptions -> PdfResult [T.Text]
pageParagraphs doc ref opts = fmap (layoutParagraphsWith opts) (pageItems doc ref)

data PageRegion = PageRegion
  { regionPage      :: !Int
  , regionParagraph :: !Int
  , regionBBox      :: !Rect
  , regionText      :: !T.Text
  } deriving (Eq, Show)

-- | Per-page paragraph regions without document-level cross-page merge.
pageRegions :: Document -> PageRef -> LayoutOptions -> PdfResult [PageRegion]
pageRegions doc ref opts = do
  items <- pageItems doc ref
  pageNo <- pageNumberOf doc ref
  let texts = layoutParagraphsWith opts items
      groups = pageItemParagraphGroups opts items
  return
    [ PageRegion pageNo idx (paraBBox grp) txt
    | (idx, (txt, grp)) <- zip [1 ..] (zip texts groups)
    ]

pageRefs :: Document -> PdfResult [PageRef]
pageRefs doc = do
  root <- docRootRef doc
  return (pageRefsFromRoot root (docObjs doc))

pageNumberOf :: Document -> PageRef -> PdfResult Int
pageNumberOf doc ref = do
  refs <- pageRefs doc
  case elemIndex ref refs of
    Nothing -> Left (MissingObject ref)
    Just i  -> Right (i + 1)

pageRefsFromRoot :: Int -> PDFObjIndex -> [PageRef]
pageRefsFromRoot parent objs =
  case findObjsByRef parent objs of
    Just os -> case findDictOfType "/Catalog" os of
      Just dict -> case findPages dict of
        Just pr -> pageRefsFromRoot pr objs
        Nothing -> []
      Nothing -> case findDictOfType "/Pages" os of
        Just dict -> case findKids dict of
          Just kidsrefs -> concatMap (\k -> pageRefsFromRoot k objs) kidsrefs
          Nothing -> []
        Nothing -> case findDictOfType "/Page" os of
          Just _ -> [parent]
          Nothing -> []
    Nothing -> []

paraBBox :: [Line] -> Rect
paraBBox [] = Rect 0 0 0 0
paraBBox ls =
  Rect
    { rectX0 = minimum (map lineInlineStart ls)
    , rectY0 = minimum (map (\l -> lineBaseline l - lineSize l) ls)
    , rectX1 = maximum (map lineInlineEnd ls)
    , rectY1 = maximum (map lineBaseline ls)
    }
