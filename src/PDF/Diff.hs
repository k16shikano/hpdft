{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Diff
Description : Paragraph-level text diff between two PDF documents
License     : MIT

Compare two opened documents page by page using the same paragraph layout as
'PDF.Page.pageParagraphs'. Emits 'TextChange' records (and optionally a
'PageCountMismatch' when page counts differ). Paragraph equality ignores
whitespace by default.

@example
import PDF.Diff (compareDocuments)
import PDF.Layout (defaultLayoutOptions)

changes <- compareDocuments defaultLayoutOptions docA docB
-}
module PDF.Diff
  ( TextChange(..)
  , DiffPipeline(..)
  , DiffColor(..)
  , compareDocuments
  , compareDocumentsWith
  , diffParagraphs
  , legacyTextParagraphs
  , alignChangeSpans
  , renderUnifiedDiff
  ) where

import PDF.Document (Document)
import PDF.Error (PdfResult)
import PDF.Layout (LayoutOptions)
import PDF.Page (pageCount, pageRefAt, pageParagraphs)
import PDF.Text (pageLegacyText)

import Data.Char (isSpace)
import Data.List (intercalate, nub, sort, sortOn)
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

data DiffPipeline
  = DiffGeom !LayoutOptions
  | DiffLegacy

-- | Whether human-readable unified diff should include ANSI colors.
data DiffColor
  = DiffColorOff
  | DiffColorAnsi
  deriving (Eq, Show)

-- | Paragraph-level diff using geometry layout (default).
compareDocuments :: LayoutOptions -> Document -> Document -> PdfResult [TextChange]
compareDocuments opts = compareDocumentsWith (DiffGeom opts)

-- | Paragraph-level diff; pipeline selects geometry vs legacy stream-order text.
compareDocumentsWith :: DiffPipeline -> Document -> Document -> PdfResult [TextChange]
compareDocumentsWith pipeline docA docB = do
  nA <- pageCount docA
  nB <- pageCount docB
  let countChange =
        if nA /= nB
          then [PageCountMismatch {pagesA = nA, pagesB = nB}]
          else []
  aligned <- mapM (diffPagePair pipeline docA docB) [1 .. min nA nB]
  extraA <- mapM (onlyInA pipeline docA) [min nA nB + 1 .. nA]
  extraB <- mapM (onlyInB pipeline docB) [min nA nB + 1 .. nB]
  return (countChange ++ concat aligned ++ concat extraA ++ concat extraB)

diffPagePair :: DiffPipeline -> Document -> Document -> Int -> PdfResult [TextChange]
diffPagePair pipeline docA docB page = do
  refA <- pageRefAt docA page
  refB <- pageRefAt docB page
  parasA <- pageParagraphsFor pipeline docA refA
  parasB <- pageParagraphsFor pipeline docB refB
  return (diffParagraphsOnPage page parasA parasB)

pageParagraphsFor :: DiffPipeline -> Document -> Int -> PdfResult [T.Text]
pageParagraphsFor (DiffGeom opts) doc ref = pageParagraphs doc ref opts
pageParagraphsFor DiffLegacy doc ref = do
  txt <- pageLegacyText doc ref
  return (legacyTextParagraphs txt)

onlyInA :: DiffPipeline -> Document -> Int -> PdfResult [TextChange]
onlyInA pipeline doc page = do
  ref <- pageRefAt doc page
  paras <- pageParagraphsFor pipeline doc ref
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

onlyInB :: DiffPipeline -> Document -> Int -> PdfResult [TextChange]
onlyInB pipeline doc page = do
  ref <- pageRefAt doc page
  paras <- pageParagraphsFor pipeline doc ref
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

-- | Split legacy page text into paragraph-sized units (blank-line separated).
legacyTextParagraphs :: T.Text -> [T.Text]
legacyTextParagraphs t =
  let chunks = filter (not . T.null) $ map collapseParaWS $ T.splitOn "\n\n" t
  in if null chunks && not (T.null (collapseParaWS t))
     then [collapseParaWS t]
     else chunks

diffParagraphsOnPage :: Int -> [T.Text] -> [T.Text] -> [TextChange]
diffParagraphsOnPage page parasA parasB =
  map attachPage (diffParagraphs parasA parasB)
  where
    attachPage TextChange{changeParaA = pa, changeParaB = pb, changeOld = old, changeNew = new} =
      TextChange (Just page) (Just page) pa pb old new
    attachPage other = other

-- | Paragraph LCS diff without page numbers (for unit tests).
--
-- When consecutive paragraphs both change, prefer a paired replace over
-- independent delete+insert. Otherwise LCS + adjacent merge can cross-wire
-- neighbors (e.g. old para N vs new para N-1).
diffParagraphs :: [T.Text] -> [T.Text] -> [TextChange]
diffParagraphs parasA parasB =
  sortChanges $ mergeReplaceChanges $ go (length normA) (length normB) []
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
      | i > 0 && j > 0 && preferReplace i j =
          go (i - 1) (j - 1)
            ( TextChange Nothing Nothing (Just (i - 1)) (Just (j - 1))
                (parasA !! (i - 1)) (parasB !! (j - 1))
                : acc
            )
      | j > 0 && (i == 0 || tableAt (i - 1) j < tableAt i (j - 1)) =
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

    -- Substitution keeps the remaining LCS; pure insert or delete would not improve it.
    preferReplace i j =
      let diag = tableAt (i - 1) (j - 1)
      in diag >= tableAt (i - 1) j && diag >= tableAt i (j - 1)

sortChanges :: [TextChange] -> [TextChange]
sortChanges =
  sortOn
    (\c -> case c of
       TextChange{changeParaA = pa, changeParaB = pb} -> (pa, pb)
       PageCountMismatch{} -> (Nothing, Nothing))

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

-- | Comparison key for a paragraph. Whitespace is ignored: PDF extractors
-- often disagree on Latin/CJK spacing while the visible text is the same.
-- Display text in 'TextChange' stays as extracted.
normalizePara :: T.Text -> T.Text
normalizePara = T.filter (not . isSpace)

-- | Soft-normalize legacy paragraph chunks for display (strip + collapse runs).
collapseParaWS :: T.Text -> T.Text
collapseParaWS = collapseInternalWS . T.strip
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

-- | Shared prefix / differing middles / shared suffix for two paragraph texts.
-- Newlines are flattened to spaces so each change stays one unified-diff line.
alignChangeSpans :: T.Text -> T.Text -> (T.Text, T.Text, T.Text, T.Text)
alignChangeSpans old0 new0 =
  let old = flattenOneLine old0
      new = flattenOneLine new0
      (pre, oldRest, newRest) = splitCommonPrefix old new
      (suf, oldMid, newMid) = splitCommonSuffix oldRest newRest
  in (pre, oldMid, newMid, suf)

flattenOneLine :: T.Text -> T.Text
flattenOneLine = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

splitCommonPrefix :: T.Text -> T.Text -> (T.Text, T.Text, T.Text)
splitCommonPrefix a b =
  let n = length (takeWhile id (zipWith (==) (T.unpack a) (T.unpack b)))
  in (T.take n a, T.drop n a, T.drop n b)

splitCommonSuffix :: T.Text -> T.Text -> (T.Text, T.Text, T.Text)
splitCommonSuffix a b =
  let ra = T.reverse a
      rb = T.reverse b
      n = length (takeWhile id (zipWith (==) (T.unpack ra) (T.unpack rb)))
  in (T.takeEnd n a, T.dropEnd n a, T.dropEnd n b)

-- | Context characters kept on each side of the changed span; longer common
-- sides are elided with an ellipsis so editor -/+ lines stay scannable.
spanContextChars :: Int
spanContextChars = 12

trimSpanContext :: T.Text -> T.Text -> (T.Text, T.Text)
trimSpanContext pre suf =
  ( trimPre pre
  , trimSuf suf
  )
  where
    trimPre t
      | T.length t <= spanContextChars = t
      | otherwise = T.cons '\x2026' (T.takeEnd spanContextChars t)
    trimSuf t
      | T.length t <= spanContextChars = t
      | otherwise = T.take spanContextChars t `T.append` T.singleton '\x2026'

-- | Unified-diff style rendering for CLI / @.diff@ files.
--
-- Structure uses @---@ / @+++@ / @\@\@@ / @-@ / @+@ so editor syntax
-- highlighters treat it as a diff. Consecutive paragraph changes on the
-- same page are merged into one hunk; within a hunk all deletions come
-- before all additions (classic multi-line replace), not interleaved
-- per paragraph. With 'DiffColorAnsi', only the changed span inside each
-- line is colored (TTY); redirected files should use 'DiffColorOff'.
renderUnifiedDiff :: DiffColor -> FilePath -> FilePath -> [TextChange] -> String
renderUnifiedDiff _ _ _ [] = ""
renderUnifiedDiff color fileA fileB changes =
  intercalate "\n" (headerLines ++ concatMap (renderHunk color) (groupDiffHunks changes)) ++ "\n"
  where
    headerLines =
      [ paint color meta ("--- " ++ fileA)
      , paint color meta ("+++ " ++ fileB)
      ]

-- | Merge consecutive same-page 'TextChange's into one hunk.
groupDiffHunks :: [TextChange] -> [[TextChange]]
groupDiffHunks [] = []
groupDiffHunks (c@(PageCountMismatch{}) : rest) =
  [c] : groupDiffHunks rest
groupDiffHunks (c@TextChange{} : rest) =
  let (peers, rest') = span (sameHunkPage c) rest
  in (c : peers) : groupDiffHunks rest'

sameHunkPage :: TextChange -> TextChange -> Bool
sameHunkPage TextChange{changePageA = pa, changePageB = pb}
             TextChange{changePageA = pa', changePageB = pb'} =
  pa == pa' && pb == pb'
sameHunkPage _ _ = False

renderHunk :: DiffColor -> [TextChange] -> [String]
renderHunk color [PageCountMismatch pa pb] =
  [ paint color hunk "@@ page count @@"
  , paintDel color (show pa ++ " pages")
  , paintAdd color (show pb ++ " pages")
  ]
renderHunk _ (PageCountMismatch{} : _) =
  -- PageCountMismatch is always alone; defensive fallback.
  []
renderHunk color cs@(TextChange{changePageA = pa, changePageB = pb} : _) =
  let bodies = map (changeLines color) cs
      dels = concatMap fst bodies
      adds = concatMap snd bodies
  in paint color hunk ("@@ " ++ hunkLocation pa pb cs ++ " @@")
       : (dels ++ adds)
renderHunk _ _ = []

-- | @('-' lines, '+' lines)@ for one paragraph change.
changeLines :: DiffColor -> TextChange -> ([String], [String])
changeLines _ PageCountMismatch{} = ([], [])
changeLines color TextChange{changeOld = old, changeNew = new}
  | T.null old = ([], [paintAdd color (T.unpack (flattenOneLine new))])
  | T.null new = ([paintDel color (T.unpack (flattenOneLine old))], [])
  | otherwise =
      let (pre0, oldMid, newMid, suf0) = alignChangeSpans old new
          (pre, suf) = trimSpanContext pre0 suf0
      in ( [paintSpan color '-' pre oldMid suf]
         , [paintSpan color '+' pre newMid suf]
         )

hunkLocation :: Maybe Int -> Maybe Int -> [TextChange] -> String
hunkLocation pa pb cs =
  case (pa, pb) of
    (Just a, Just b) | a == b ->
      "page " ++ show a ++ ", " ++ paraLabel
    (Just a, Just b) ->
      "page " ++ show a ++ " vs " ++ show b ++ ", " ++ paraLabel
    (Just a, Nothing) ->
      "page " ++ show a ++ " (only in first), " ++ paraLabel
    (Nothing, Just b) ->
      "page " ++ show b ++ " (only in second), " ++ paraLabel
    _ -> paraLabel
  where
    paraLabel = formatParaRefs [ (pxa, pxb) | TextChange{changeParaA = pxa, changeParaB = pxb} <- cs ]

-- | Compact 1-based paragraph references: @para 6@, @para 6-9@, @para 6, 8-11@.
formatParaRefs :: [(Maybe Int, Maybe Int)] -> String
formatParaRefs refs =
  case collect of
    [] -> "para ?"
    xs -> "para " ++ intercalate ", " (map formatRun (groupRuns xs))
  where
    collect = sort $ nub [ n | (ma, mb) <- refs, n <- maybePara ma mb ]
    maybePara (Just a) (Just b)
      | a == b = [a + 1]
      | otherwise = [a + 1, b + 1]
    maybePara (Just a) Nothing = [a + 1]
    maybePara Nothing (Just b) = [b + 1]
    maybePara _ _ = []

    groupRuns [] = []
    groupRuns (x : xs) = go [x] xs
      where
        go run [] = [reverse run]
        go run@(r : _) (y : ys)
          | y == r + 1 = go (y : run) ys
          | otherwise = reverse run : go [y] ys
        go _ _ = []

    formatRun [a] = show a
    formatRun (a : rest) = show a ++ "-" ++ show (last (a : rest))
    formatRun [] = "?"

paintSpan :: DiffColor -> Char -> T.Text -> T.Text -> T.Text -> String
paintSpan DiffColorOff mark pre mid suf =
  mark : T.unpack (pre `T.append` mid `T.append` suf)
paintSpan DiffColorAnsi mark pre mid suf =
  let midColor = if mark == '-' then red else green
  in mark
       : dim (T.unpack pre)
      ++ midColor (T.unpack mid)
      ++ dim (T.unpack suf)
      ++ reset

paintDel :: DiffColor -> String -> String
paintDel DiffColorOff s = '-' : s
paintDel DiffColorAnsi s = '-' : red s ++ reset

paintAdd :: DiffColor -> String -> String
paintAdd DiffColorOff s = '+' : s
paintAdd DiffColorAnsi s = '+' : green s ++ reset

paint :: DiffColor -> (String -> String) -> String -> String
paint DiffColorOff _ s = s
paint DiffColorAnsi style s = style s ++ reset

meta, hunk, red, green, dim :: String -> String
meta s = "\ESC[1m" ++ s
hunk s = "\ESC[36m" ++ s
red s = "\ESC[31m" ++ s
green s = "\ESC[32m" ++ s
dim s = "\ESC[2m" ++ s

reset :: String
reset = "\ESC[0m"
