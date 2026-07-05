{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Diff
Description : Paragraph-level text diff between two PDF documents
License     : MIT
-}
module PDF.Diff
  ( TextChange(..)
  , compareDocuments
  , diffParagraphs
  ) where

import PDF.Document (Document)
import PDF.Error (PdfResult)
import PDF.Layout (LayoutOptions)
import PDF.Page (pageCount, pageRefAt, pageParagraphs)

import Data.Char (isSpace)
import qualified Data.Text as T

data TextChange
  = TextChange
    { changePageA      :: !(Maybe Int)
    , changePageB      :: !(Maybe Int)
    , changeParaA      :: !(Maybe Int)
    , changeParaB      :: !(Maybe Int)
    , changeOld        :: !T.Text
    , changeNew        :: !T.Text
    }
  | PageCountMismatch
    { pagesA :: !Int
    , pagesB :: !Int
    }
  deriving (Eq, Show)

compareDocuments :: LayoutOptions -> Document -> Document -> PdfResult [TextChange]
compareDocuments opts docA docB = do
  nA <- pageCount docA
  nB <- pageCount docB
  let countChange =
        if nA /= nB
          then [PageCountMismatch {pagesA = nA, pagesB = nB}]
          else []
  aligned <- mapM (diffPagePair opts docA docB) [1 .. min nA nB]
  extraA <- mapM (onlyInA opts docA) [min nA nB + 1 .. nA]
  extraB <- mapM (onlyInB opts docB) [min nA nB + 1 .. nB]
  return (countChange ++ concat aligned ++ concat extraA ++ concat extraB)

diffPagePair :: LayoutOptions -> Document -> Document -> Int -> PdfResult [TextChange]
diffPagePair opts docA docB page = do
  refA <- pageRefAt docA page
  refB <- pageRefAt docB page
  parasA <- pageParagraphs docA refA opts
  parasB <- pageParagraphs docB refB opts
  return (diffParagraphsOnPage page parasA parasB)

onlyInA :: LayoutOptions -> Document -> Int -> PdfResult [TextChange]
onlyInA opts doc page = do
  ref <- pageRefAt doc page
  paras <- pageParagraphs doc ref opts
  return
    [ TextChange
        { changePageA = Just page
        , changePageB = Nothing
        , changeParaA = Just idx
        , changeParaB = Nothing
        , changeOld = txt
        , changeNew = T.empty
        }
    | (idx, txt) <- zip [0 ..] paras
    ]

onlyInB :: LayoutOptions -> Document -> Int -> PdfResult [TextChange]
onlyInB opts doc page = do
  ref <- pageRefAt doc page
  paras <- pageParagraphs doc ref opts
  return
    [ TextChange
        { changePageA = Nothing
        , changePageB = Just page
        , changeParaA = Nothing
        , changeParaB = Just idx
        , changeOld = T.empty
        , changeNew = txt
        }
    | (idx, txt) <- zip [0 ..] paras
    ]

diffParagraphsOnPage :: Int -> [T.Text] -> [T.Text] -> [TextChange]
diffParagraphsOnPage page parasA parasB =
  map attachPage (diffParagraphs parasA parasB)
  where
    attachPage TextChange{changeParaA = pa, changeParaB = pb, changeOld = old, changeNew = new} =
      TextChange (Just page) (Just page) pa pb old new
    attachPage other = other

-- | Paragraph LCS diff without page numbers (for unit tests).
diffParagraphs :: [T.Text] -> [T.Text] -> [TextChange]
diffParagraphs parasA parasB =
  mergeReplaceChanges $ go (length normA) (length normB) []
  where
    normA = map normalizePara parasA
    normB = map normalizePara parasB
    nB = length normB
    table = lcsTable normA normB
    tableAt i j = table !! (i * (nB + 1) + j)

    go 0 0 acc = reverse acc
    go i j acc
      | i > 0 && j > 0 && normA !! (i - 1) == normB !! (j - 1) =
          go (i - 1) (j - 1) acc
      | j > 0 && (i == 0 || tableAt (i - 1) j <= tableAt i (j - 1)) =
          go i (j - 1)
            ( TextChange Nothing Nothing Nothing (Just (j - 1)) T.empty (parasB !! (j - 1))
                : acc
            )
      | i > 0 =
          go (i - 1) j
            ( TextChange Nothing Nothing (Just (i - 1)) Nothing (parasA !! (i - 1)) T.empty
                : acc
            )
      | otherwise = reverse acc

mergeReplaceChanges :: [TextChange] -> [TextChange]
mergeReplaceChanges [] = []
mergeReplaceChanges (c : d : rest)
  | isRemoval c && isAddition d =
      mergedChange c d : mergeReplaceChanges rest
  | isAddition c && isRemoval d =
      mergedChange d c : mergeReplaceChanges rest
  | otherwise = c : mergeReplaceChanges (d : rest)
mergeReplaceChanges [c] = [c]

mergedChange :: TextChange -> TextChange -> TextChange
mergedChange TextChange{changeParaA = pa, changeOld = old}
             TextChange{changeParaB = pb, changeNew = new} =
  TextChange Nothing Nothing pa pb old new

isRemoval :: TextChange -> Bool
isRemoval TextChange{changeOld = old, changeNew = new} =
  not (T.null old) && T.null new
isRemoval _ = False

isAddition :: TextChange -> Bool
isAddition TextChange{changeOld = old, changeNew = new} =
  T.null old && not (T.null new)
isAddition _ = False

normalizePara :: T.Text -> T.Text
normalizePara = collapseInternalWS . T.strip
  where
    collapseInternalWS t =
      T.pack $ go False (T.unpack t)
    go _ [] = []
    go _ ('\r' : cs) = go False cs
    go seen ('\n' : cs) = if seen then go True cs else ' ' : go True cs
    go seen (c : cs)
      | isSpace c = if seen then go True cs else ' ' : go True cs
      | otherwise = c : go False cs

lcsTable :: Eq a => [a] -> [a] -> [Int]
lcsTable xs ys =
  let m = length xs
      n = length ys
      row i j
        | i == 0 || j == 0 = 0
        | xs !! (i - 1) == ys !! (j - 1) =
            1 + tableAt (i - 1) (j - 1)
        | otherwise = max (tableAt (i - 1) j) (tableAt i (j - 1))
      tableAt i j = table !! (i * (n + 1) + j)
      table = [row i j | i <- [0 .. m], j <- [0 .. n]]
   in table
