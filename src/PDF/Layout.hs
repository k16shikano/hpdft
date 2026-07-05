{-# LANGUAGE OverloadedStrings #-}

module PDF.Layout
  ( Rect(..)
  , PageItem(..)
  , Line(..)
  , LayoutOptions(..)
  , defaultLayoutOptions
  , layoutParagraphs
  , layoutParagraphsWith
  , layoutPageText
  , layoutPageTextWith
  , layoutDocument
  , layoutDocumentWith
  , stripHeadersFooters
  , linesFromGlyphs
  , joinParaLines
  , intraLineSpace
  , joinGlyphsRun
  ) where

import PDF.Interpret (Glyph(..), Rect(..), PageItem(..))

import Data.Char (isSpace, ord)
import Data.List (foldl', maximumBy, sort)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- Footnote bodies continuing onto the following page are not merged.
data LayoutOptions = LayoutOptions
  { optFootnotes :: Bool
  } deriving (Eq, Show)

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions {optFootnotes = False}

layoutPageText :: [PageItem] -> T.Text
layoutPageText = layoutPageTextWith defaultLayoutOptions

layoutPageTextWith :: LayoutOptions -> [PageItem] -> T.Text
layoutPageTextWith opts items = formatParagraphs (layoutParagraphsWith opts items)

layoutDocument :: [[PageItem]] -> T.Text
layoutDocument = layoutDocumentWith defaultLayoutOptions

layoutDocumentWith :: LayoutOptions -> [[PageItem]] -> T.Text
layoutDocumentWith opts pages = formatParagraphs (documentParagraphs opts pages)

formatParagraphs :: [T.Text] -> T.Text
formatParagraphs ps =
  if null ps
  then "\n"
  else T.intercalate "\n\n" ps `T.append` "\n"

layoutParagraphs :: [PageItem] -> [T.Text]
layoutParagraphs = layoutParagraphsWith defaultLayoutOptions

layoutParagraphsWith :: LayoutOptions -> [PageItem] -> [T.Text]
layoutParagraphsWith opts items =
  case applyFootnotes opts (pageLines items) of
    PageFallback ps -> ps
    PageNormal wmode graphics bounds ls ->
      map joinParaLines (groupParagraphs wmode graphics bounds ls)

documentParagraphs :: LayoutOptions -> [[PageItem]] -> [T.Text]
documentParagraphs opts pages =
  let pageCount = length pages
      layouts = map pageLines pages
      stripped = applyHeaderFooterStrip pageCount layouts
      final = map (applyFootnotes opts) stripped
  in finalizeDoc $ foldl' processPage ([], []) final
  where
    applyHeaderFooterStrip n layouts =
      let normalPairs = [(i, ls) | (i, PageNormal _ _ _ ls) <- zip [0 ..] layouts]
          strippedNormals = stripHeadersFooters n (map snd normalPairs)
          strippedMap = M.fromList (zip (map fst normalPairs) strippedNormals)
      in [ case layout of
             PageFallback ps -> PageFallback ps
             PageNormal w g b ls ->
               PageNormal w g b (M.findWithDefault ls i strippedMap)
         | (i, layout) <- zip [0 ..] layouts
         ]

    continuePage done pageGroups =
      case reverse pageGroups of
        [] -> (done, [])
        lastG : restRev -> (done ++ map joinParaLines (reverse restRev), lastG)

    processPage (done, pending) (PageFallback ps) =
      ( done ++ finalize pending ++ map T.strip ps
      , []
      )

    processPage (done, pending) (PageNormal wmode graphics bounds ls) =
      let pageGroups = groupParagraphs wmode graphics bounds ls
          pageMinInline =
            if null ls
            then 0
            else minimum (map lineInlineStart ls)
      in case (pending, pageGroups) of
        ([], _) -> continuePage done pageGroups
        (ps, []) -> (done, ps)
        (ps, g : gs) ->
          case g of
            firstLine : _ ->
              let paraSoFar = joinParaLines ps
                  lastLine = case reverse ps of
                    l : _ -> l
                    [] -> firstLine
              in if pageBoundaryBreak paraSoFar firstLine pageMinInline lastLine firstLine
                 then continuePage (done ++ [paraSoFar]) (g : gs)
                 else case reverse gs of
                        [] -> (done, ps ++ g)
                        lastG : restRev ->
                          ( done ++ joinParaLines (ps ++ g)
                              : map joinParaLines (reverse restRev)
                          , lastG
                          )
            [] -> (done, ps)

    finalizeDoc (done, pending) = done ++ finalize pending

    finalize [] = []
    finalize ps = [joinParaLines ps]

data PageLines
  = PageFallback [T.Text]
  | PageNormal Int [Rect] (Double, Double) [Line]
  deriving (Show)

applyFootnotes :: LayoutOptions -> PageLines -> PageLines
applyFootnotes opts page =
  case page of
    PageNormal 0 graphics bounds ls
      | optFootnotes opts ->
          PageNormal 0 graphics bounds (inlineFootnotes graphics ls)
    _ -> page

-- Footnote heuristic (horizontal pages only): small-size lines in the
-- bottom band (or under a bottom horizontal rule) that start with a
-- marker such as †2 form footnote blocks; matching superscript markers
-- in the body are replaced with <footnote>...</footnote>.
inlineFootnotes :: [Rect] -> [Line] -> [Line]
inlineFootnotes graphics ls
  | null ls = ls
  | otherwise =
      let bodySize = medianOf (map lineSize ls)
          (lo, hi) = pageBaselineExtent ls
          bandTop = lo + 0.35 * (hi - lo)
          ruleYs =
            [ max (rectY0 r) (rectY1 r)
            | r <- graphics
            , rectHeight r < 1
            , rectWidth r >= 40
            , min (rectY0 r) (rectY1 r) <= bandTop
            ]
          isSmall l = lineSize l <= 0.85 * bodySize
          inRegion l =
            isSmall l
            && (lineBaseline l <= bandTop || any (> lineBaseline l) ruleYs)
          tagged = [(inRegion l, l) | l <- ls]
          regionLines = [l | (True, l) <- tagged]
          blocks = footnoteBlocks regionLines
          (consumedIdx, replaceBody) = matchAnchors blocks [l | (False, l) <- tagged]
          consumedLines = S.fromList
            [ i
            | (bi, (_, _, lineIdxs)) <- zip [0 :: Int ..] blocks
            , bi `S.member` consumedIdx
            , i <- lineIdxs
            ]
          keep (i, (inR, l))
            | inR = not (regionIndexOf i `S.member` consumedLines)
            | otherwise = True
          regionIndexOf i =
            length [() | (j, (True, _)) <- zip [0 ..] tagged, j < i]
      in [ if inR then l else replaceBody l
         | (i, (inR, l)) <- zip [0 :: Int ..] tagged
         , keep (i, (inR, l))
         ]

-- Blocks: (marker key, body text, indexes into the region line list).
footnoteBlocks :: [Line] -> [(T.Text, T.Text, [Int])]
footnoteBlocks regionLines = go (zip [0 ..] regionLines)
  where
    go [] = []
    go ((i, l) : rest) =
      case blockStart l of
        Nothing -> go rest
        Just (key, firstText) ->
          let (cont, rest') = break (\(_, l') -> blockStart l' /= Nothing) rest
              bodyLines = firstText : map (T.strip . lineText . snd) cont
              body = T.strip (foldl' cjkJoin T.empty bodyLines)
          in (key, body, i : map fst cont) : go rest'

    cjkJoin a b
      | T.null a = b
      | T.null b = a
      | isCJK (T.last a) && isCJK (T.head b) = a `T.append` b
      | otherwise = a `T.append` " " `T.append` b

blockStart :: Line -> Maybe (T.Text, T.Text)
blockStart l =
  case [mt | (0, mt) <- lineMarkers l] of
    mt : _ | Just key <- markerKey mt ->
      Just (key, T.strip (T.drop (T.length mt) (lineText l)))
    _ ->
      let t = T.stripStart (lineText l)
      in case T.uncons t of
           Just (c, rest)
             | c `elem` ("\8224\8225*\8251" :: String) ->
                 let (ds, rest') = T.span isAsciiDigit rest
                 in if not (T.null ds) && T.length ds <= 3
                    then Just (T.cons c ds, T.strip rest')
                    else Nothing
           _ -> Nothing

-- Anchor pass: returns consumed block indexes and a body-line rewriter.
matchAnchors :: [(T.Text, T.Text, [Int])] -> [Line] -> (S.Set Int, Line -> Line)
matchAnchors blocks bodyLines =
  let anchors =
        [ key
        | l <- bodyLines
        , (_, mt) <- lineMarkers l
        , Just key <- [markerKey mt]
        ]
      assign consumed [] = consumed
      assign consumed (key : rest) =
        case [ bi
             | (bi, (bkey, _, _)) <- zip [0 ..] blocks
             , bkey == key
             , not (bi `S.member` S.map fst consumed)
             ] of
          bi : _ -> assign (S.insert (bi, key) consumed) rest
          [] -> assign consumed rest
      consumedPairs = assign S.empty anchors
      consumedIdx = S.map fst consumedPairs
      bodyOf key =
        case [ (bi, b)
             | (bi, (bkey, b, _)) <- zip [0 ..] blocks
             , bkey == key
             , bi `S.member` consumedIdx
             ] of
          (_, b) : _ -> Just b
          [] -> Nothing
      rewrite l =
        let (txt', finalPos, _) =
              foldl' step (T.empty, 0, S.empty) (lineMarkers l)
            step (acc, pos, used) (off, mt) =
              let pre = T.take (off - pos) (T.drop pos (lineText l))
                  after = off + T.length mt
              in case markerKey mt of
                   Just key
                     | not (key `S.member` used)
                     , Just b <- bodyOf key ->
                         ( T.concat [acc, pre, "<footnote>", b, "</footnote>"]
                         , after
                         , S.insert key used
                         )
                   _ -> (acc `T.append` pre `T.append` mt, after, used)
            rest = T.drop finalPos (lineText l)
        in if null (lineMarkers l)
           then l
           else l {lineText = txt' `T.append` rest, lineMarkers = []}
  in (consumedIdx, rewrite)

markerKey :: T.Text -> Maybe T.Text
markerKey mt =
  let s = T.filter (not . isSpace) mt
  in case T.uncons s of
       Just (c, rest)
         | c `elem` ("\8224\8225*\8251" :: String)
         , digits rest -> Just s
       _ | digits s -> Just s
       _ -> Nothing
  where
    digits d = not (T.null d) && T.length d <= 3 && T.all isAsciiDigit d

isAsciiDigit :: Char -> Bool
isAsciiDigit c = c >= '0' && c <= '9'

medianOf :: [Double] -> Double
medianOf xs =
  case sort xs of
    [] -> 0
    sorted ->
      let n = length sorted
          mid = n `div` 2
      in if odd n
         then sorted !! mid
         else (sorted !! (mid - 1) + sorted !! mid) / 2

pageLines :: [PageItem] -> PageLines
pageLines items =
  let glyphs = [g | ItemGlyph g <- items]
      graphics = [r | ItemGraphic r <- items]
  in if null glyphs
     then PageFallback []
     else if fallbackNeeded glyphs
          then PageFallback [fallbackText glyphs]
          else let wmode = dominantWMode glyphs
                   pageBounds = pageExtents glyphs
                   ls = buildLines glyphs
               in PageNormal wmode graphics pageBounds ls

linesFromGlyphs :: [Glyph] -> [Line]
linesFromGlyphs = buildLines

stripHeadersFooters :: Int -> [[Line]] -> [[Line]]
stripHeadersFooters pageCount pagesLines =
  let threshold =
        let raw = ceiling (0.2 * fromIntegral pageCount :: Double) :: Int
        in max 3 (min raw 5)
      pageInfos =
        [ (ls, pageBaselineExtent ls)
        | ls <- pagesLines
        , not (null ls)
        ]
      topCounts = countBandCores Top pageInfos
      bottomCounts = countBandCores Bottom pageInfos
      repeatedTop = repeatedCores threshold pageCount topCounts
      repeatedBottom = repeatedCores threshold pageCount bottomCounts
  in map (filterLine repeatedTop repeatedBottom) pagesLines
  where
    filterLine repTop repBottom ls
      | length ls <= 2 =
          let extent = pageBaselineExtent ls
          in if any (isRemoved repTop repBottom extent) ls
             then filter (not . isRemoved repTop repBottom extent) ls
             else ls
      | otherwise =
          let extent = pageBaselineExtent ls
          in filter (not . isRemoved repTop repBottom extent) ls

    isRemoved repTop repBottom extent l =
      let band = lineBand extent l
          norm = normalizeHeaderFooterText (lineText l)
      in shouldRemove band norm pageCount repTop repBottom

    repeatedCores thresh n counts
      | n >= 3 =
          S.fromList [ core | (core, c) <- M.toList counts, c >= thresh ]
      | otherwise = S.empty

countBandCores :: Band -> [([Line], (Double, Double))] -> M.Map T.Text Int
countBandCores band pageInfos =
  M.fromListWith (+)
    [ (headerFooterCore (lineText l), 1)
    | (ls, extent) <- pageInfos
    , l <- ls
    , lineBand extent l == band
    ]

headerFooterCore :: T.Text -> T.Text
headerFooterCore = T.filter (/= '#') . normalizeHeaderFooterText

shouldRemove :: Band -> T.Text -> Int -> S.Set T.Text -> S.Set T.Text -> Bool
shouldRemove band norm pageCount repTop repBottom
  | band == Middle = False
  | isBarePageNumber norm = pageCount >= 2
  | otherwise =
      let core = headerFooterCore norm
          repeated = case band of
            Top -> repTop
            Bottom -> repBottom
            Middle -> S.empty
      in S.member core repeated

data Band = Top | Bottom | Middle
  deriving (Eq, Ord, Show)

lineBand :: (Double, Double) -> Line -> Band
lineBand (lo, hi) l =
  let bl = lineBaseline l
      span = hi - lo
  in if span <= 0
     then Middle
     else if bl >= hi - 0.15 * span
          then Top
          else if bl <= lo + 0.15 * span
               then Bottom
               else Middle

pageBaselineExtent :: [Line] -> (Double, Double)
pageBaselineExtent ls =
  let baselines = map lineBaseline ls
  in (minimum baselines, maximum baselines)

normalizeHeaderFooterText :: T.Text -> T.Text
normalizeHeaderFooterText =
  replaceRomanNumerals . replaceAsciiDigits . T.filter (not . isSpace)

replaceAsciiDigits :: T.Text -> T.Text
replaceAsciiDigits t =
  let chars = T.unpack t
      (out, _) = foldl' go ([], False) chars
  in T.pack (reverse out)
  where
    go (acc, inRun) c
      | c >= '0' && c <= '9' =
          if inRun then (acc, True) else ('#' : acc, True)
      | otherwise = (c : acc, False)

replaceRomanNumerals :: T.Text -> T.Text
replaceRomanNumerals t = T.pack (go (T.unpack t) [])
  where
    go [] acc = reverse acc
    go (c : cs') acc =
      let (tok, rest) = span isRomanDigit (c : cs')
      in if not (null tok) && length tok <= 7
         then go rest ('#' : acc)
         else go cs' (c : acc)

isRomanDigit :: Char -> Bool
isRomanDigit c = c `elem` ("ivxlcdmIVXLCDM" :: String)

isBarePageNumber :: T.Text -> Bool
isBarePageNumber t =
  not (T.null t)
  && T.any (=='#') t
  && T.all (\c -> c == '#' || c `elem` ("-/." :: String)) t

pageBoundaryBreak :: T.Text -> Line -> Double -> Line -> Line -> Bool
pageBoundaryBreak paraSoFar firstLine pageMinInline lastLine firstLine' =
  endsWithTerminal paraSoFar
  || indentPageBreak pageMinInline firstLine
  || abs (lineSize firstLine' - lineSize lastLine)
       > 0.15 * max (lineSize firstLine') (lineSize lastLine)
  || lineWMode lastLine /= lineWMode firstLine'

fallbackNeeded :: [Glyph] -> Bool
fallbackNeeded glyphs =
  let n = length glyphs
      usable = length (filter usableGlyph glyphs)
  in n == 0 || fromIntegral usable / fromIntegral n < 0.7

fallbackText :: [Glyph] -> T.Text
fallbackText = T.intercalate "\n" . map glyphText

usableGlyph :: Glyph -> Bool
usableGlyph g =
  glyphSize g > 0
  && not (isNaN (glyphX g) || isInfinite (glyphX g))
  && not (isNaN (glyphY g) || isInfinite (glyphY g))

dominantWMode :: [Glyph] -> Int
dominantWMode glyphs =
  let counts = foldr (\g m -> M.insertWith (+) (glyphWMode g) 1 m) M.empty glyphs
  in if M.null counts
     then 0
     else fst (maximumBy (comparing snd) (M.toList counts))

pageExtents :: [Glyph] -> (Double, Double)
pageExtents glyphs =
  let xs = concatMap (\g -> [glyphX g, glyphX g + glyphWidth g]) glyphs
      ys = map glyphY glyphs
  in (max 1 (maximum xs - minimum xs), max 1 (maximum ys - minimum ys))

baselineOf :: Int -> Glyph -> Double
baselineOf wmode g = if wmode == 1 then glyphX g else glyphY g

inlineStartOf :: Int -> Glyph -> Double
inlineStartOf wmode g = if wmode == 1 then glyphY g else glyphX g

inlineEndOf :: Int -> Glyph -> Double
inlineEndOf wmode g =
  if wmode == 1
  then glyphY g - glyphWidth g
  else glyphX g + glyphWidth g

data Line = Line
  { lineBaseline    :: !Double
  , lineInlineStart :: !Double
  , lineInlineEnd   :: !Double
  , lineSize        :: !Double
  , lineFirstInline :: !Double
  , lineWMode       :: !Int
  , lineText        :: !T.Text
  , lineMarkers     :: [(Int, T.Text)]
  , lineLastSuper   :: !Bool
  } deriving (Show)

buildLines :: [Glyph] -> [Line]
buildLines = reverse . foldl' go []
  where
    go [] g = [newLine g]
    go (l:ls) g
      | glyphWMode g /= lineWMode l = newLine g : l : ls
      | superAttach = mergeSuper l g : ls
      | rebaseAttach = mergeRebase l g : ls
      | abs d <= 0.4 * max (glyphSize g) (lineSize l) = mergeGlyph l g : ls
      | otherwise = newLine g : l : ls
      where
        d = baselineOf (lineWMode l) g - lineBaseline l
        gap = inlineStartOf (lineWMode l) g - lineInlineEnd l
        -- The glyph continues roughly where the line left off; a glyph
        -- jumping back to the margin is a new line, not a super/subscript.
        inlineCont refSize = gap >= -0.5 * refSize && gap <= 2.0 * refSize
        -- Superscript/subscript run (footnote markers etc.): a clearly
        -- smaller glyph with a real baseline shift riding above (or
        -- hanging slightly below) the current line stays on that line.
        superAttach =
          glyphSize g <= 0.92 * lineSize l
          && glyphSize g >= 0.5 * lineSize l
          && inlineCont (lineSize l)
          && ((d > 0.25 * lineSize l && d <= 0.75 * lineSize l)
              || ((-d) > 0.25 * lineSize l && (-d) <= 0.4 * lineSize l))
        -- A line that so far consists only of marker-sized glyphs gets
        -- rebased onto the larger body glyph that follows it (footnote
        -- bodies start with their superscript marker).
        rebaseAttach =
          lineSize l <= 0.92 * glyphSize g
          && lineSize l >= 0.5 * glyphSize g
          && inlineCont (glyphSize g)
          && (((-d) > 0.25 * glyphSize g && (-d) <= 0.75 * glyphSize g)
              || (d > 0.25 * glyphSize g && d <= 0.4 * glyphSize g))

newLine :: Glyph -> Line
newLine g =
  Line
    { lineBaseline = baselineOf (glyphWMode g) g
    , lineInlineStart = inlineStartOf (glyphWMode g) g
    , lineInlineEnd = inlineEndOf (glyphWMode g) g
    , lineSize = glyphSize g
    , lineFirstInline = inlineStartOf (glyphWMode g) g
    , lineWMode = glyphWMode g
    , lineText = glyphText g
    , lineMarkers = []
    , lineLastSuper = False
    }

mergeGlyph :: Line -> Glyph -> Line
mergeGlyph line g =
  let w = lineWMode line
      gap = inlineStartOf w g - lineInlineEnd line
      size = max (glyphSize g) (lineSize line)
      space = intraLineSpace gap size (lastChar (lineText line)) (firstChar (glyphText g))
  in line
       { lineInlineEnd = inlineEndOf w g
       , lineInlineStart = min (lineInlineStart line) (inlineStartOf w g)
       , lineSize = size
       , lineText = lineText line `T.append` space `T.append` glyphText g
       , lineLastSuper = False
       }

mergeSuper :: Line -> Glyph -> Line
mergeSuper line g =
  let w = lineWMode line
      gap = inlineStartOf w g - lineInlineEnd line
      space = intraLineSpace gap (lineSize line) (lastChar (lineText line)) (firstChar (glyphText g))
      offset = T.length (lineText line) + T.length space
      markers =
        if lineLastSuper line
        then case reverse (lineMarkers line) of
          (off, mt) : restRev ->
            reverse ((off, mt `T.append` space `T.append` glyphText g) : restRev)
          [] -> [(offset, glyphText g)]
        else lineMarkers line ++ [(offset, glyphText g)]
  in line
       { lineInlineEnd = inlineEndOf w g
       , lineInlineStart = min (lineInlineStart line) (inlineStartOf w g)
       , lineText = lineText line `T.append` space `T.append` glyphText g
       , lineMarkers = markers
       , lineLastSuper = True
       }

mergeRebase :: Line -> Glyph -> Line
mergeRebase line g =
  let w = lineWMode line
      gap = inlineStartOf w g - lineInlineEnd line
      space = intraLineSpace gap (glyphSize g) (lastChar (lineText line)) (firstChar (glyphText g))
  in line
       { lineBaseline = baselineOf w g
       , lineSize = glyphSize g
       , lineInlineEnd = inlineEndOf w g
       , lineInlineStart = min (lineInlineStart line) (inlineStartOf w g)
       , lineText = lineText line `T.append` space `T.append` glyphText g
       , lineMarkers = [(0, lineText line)]
       , lineLastSuper = False
       }

joinGlyphsRun :: [Glyph] -> T.Text
joinGlyphsRun [] = T.empty
joinGlyphsRun (g : gs) =
  let (txt, _) = foldl' go (glyphText g, g) gs
  in txt
  where
    go (acc, prev) g' =
      let wmode = glyphWMode g'
          gap = inlineStartOf wmode g' - inlineEndOf wmode prev
          size = max (glyphSize g') (glyphSize prev)
          space = intraLineSpace gap size (lastChar acc) (firstChar (glyphText g'))
      in (acc `T.append` space `T.append` glyphText g', g')

intraLineSpace :: Double -> Double -> Maybe Char -> Maybe Char -> T.Text
intraLineSpace gap size mc nc
  | gap > 2.0 * size = " "
  | gap > 0.3 * size, not (cjkAdjacent mc nc) = " "
  | otherwise = ""

lastChar :: T.Text -> Maybe Char
lastChar t = if T.null t then Nothing else Just (T.last t)

firstChar :: T.Text -> Maybe Char
firstChar t = if T.null t then Nothing else Just (T.head t)

cjkAdjacent :: Maybe Char -> Maybe Char -> Bool
cjkAdjacent (Just a) (Just b) = isCJK a && isCJK b
cjkAdjacent _ _ = False

isCJK :: Char -> Bool
isCJK c =
  let cp = ord c
  in (cp >= 0x4E00 && cp <= 0x9FFF)
  || (cp >= 0x3040 && cp <= 0x309F)
  || (cp >= 0x30A0 && cp <= 0x30FF)
  || (cp >= 0x3000 && cp <= 0x303F)
  || (cp >= 0xFF00 && cp <= 0xFFEF)

groupParagraphs :: Int -> [Rect] -> (Double, Double) -> [Line] -> [[Line]]
groupParagraphs wmode graphics bounds lines =
  go [] (filter (not . T.null . T.strip . lineText) lines)
  where
    go _ [] = []
    go pageGaps (l:ls) =
      let (para, rest, pageGaps') = takeParagraph pageGaps l ls
      in para : go pageGaps' rest

    takeParagraph pageGaps first rest =
      let (paraLines, rest', pageGaps') =
            spanLines pageGaps first rest [first] (lineInlineStart first)
      in (paraLines, rest', pageGaps')

    spanLines pageGaps prev rest acc minInline =
      case rest of
        [] -> (reverse acc, [], pageGaps)
        (l:ls) ->
          if paragraphBreak wmode graphics bounds prev l pageGaps minInline
          then (reverse acc, l:ls, pageGaps)
          else let g = baselineGap wmode prev l
                   pageGaps' = if g > 0 then pageGaps ++ [g] else pageGaps
                   minInline' = min minInline (lineInlineStart l)
               in spanLines pageGaps' l ls (l:acc) minInline'

paragraphBreak :: Int -> [Rect] -> (Double, Double) -> Line -> Line -> [Double] -> Double -> Bool
paragraphBreak wmode graphics pageBounds prev cur gaps paraMinInline =
  let gap = baselineGap wmode prev cur
      typical = typicalLeading gaps (lineSize cur)
  in negativeAdvance wmode prev cur
  || abs gap > 1.6 * typical
  || indentBreak paraMinInline cur
  || graphicBreak wmode graphics pageBounds prev cur

baselineGap :: Int -> Line -> Line -> Double
baselineGap wmode prev cur = lineBaseline prev - lineBaseline cur

negativeAdvance :: Int -> Line -> Line -> Bool
negativeAdvance wmode prev cur = baselineGap wmode prev cur < 0

typicalLeading :: [Double] -> Double -> Double
typicalLeading gaps lineSize =
  case sort (filter (> 0) gaps) of
    [] -> 1.2 * lineSize
    [_] -> 1.2 * lineSize
    gs ->
      let mid = length gs `div` 2
      in if odd (length gs)
         then gs !! mid
         else (gs !! (mid - 1) + gs !! mid) / 2

indentBreak :: Double -> Line -> Bool
indentBreak paraMinInline cur =
  lineFirstInline cur - paraMinInline >= 0.85 * lineSize cur

indentPageBreak :: Double -> Line -> Bool
indentPageBreak pageMinInline cur =
  lineFirstInline cur - pageMinInline >= 0.85 * lineSize cur

graphicBreak :: Int -> [Rect] -> (Double, Double) -> Line -> Line -> Bool
graphicBreak wmode graphics pageBounds prev cur =
  any (separatesLines wmode pageW pageH prev cur) (candidatesBetween wmode prev cur pageBounds graphics)
  where
    (pageW, pageH) = pageBounds

candidatesBetween :: Int -> Line -> Line -> (Double, Double) -> [Rect] -> [Rect]
candidatesBetween wmode prev cur (pageW, pageH) graphics =
  let tol = 0.2 * lineSize cur
      lo = min (lineBaseline prev) (lineBaseline cur) - tol
      hi = max (lineBaseline prev) (lineBaseline cur) + tol
  in filter (graphicCandidate wmode pageW pageH lo hi) graphics

graphicCandidate :: Int -> Double -> Double -> Double -> Double -> Rect -> Bool
graphicCandidate wmode pageW pageH lo hi r =
  let bigEnough = rectWidth r > 0.8 * pageW && rectHeight r > 0.8 * pageH
      tiny = rectWidth r < 0.5 && rectHeight r < 0.5
      rLo = if wmode == 1 then min (rectX0 r) (rectX1 r) else min (rectY0 r) (rectY1 r)
      rHi = if wmode == 1 then max (rectX0 r) (rectX1 r) else max (rectY0 r) (rectY1 r)
  in not bigEnough && not tiny && rLo <= hi && rHi >= lo

separatesLines :: Int -> Double -> Double -> Line -> Line -> Rect -> Bool
separatesLines wmode _pageW _pageH prev cur r =
  inlineOverlap wmode prev cur r

rectWidth :: Rect -> Double
rectWidth r = abs (rectX1 r - rectX0 r)

rectHeight :: Rect -> Double
rectHeight r = abs (rectY1 r - rectY0 r)

inlineOverlap :: Int -> Line -> Line -> Rect -> Bool
inlineOverlap wmode prev cur r =
  let unionLo = min (lineInlineStart prev) (lineInlineStart cur)
      unionHi = max (lineInlineEnd prev) (lineInlineEnd cur)
      unionLen = max 0 (unionHi - unionLo)
      (rLo, rHi) = rectInlineRange wmode r
      overlap = max 0 (min unionHi rHi - max unionLo rLo)
  in unionLen <= 0 || overlap / unionLen >= 0.2

rectInlineRange :: Int -> Rect -> (Double, Double)
rectInlineRange wmode r =
  if wmode == 1
  then (min (rectY0 r) (rectY1 r), max (rectY0 r) (rectY1 r))
  else (min (rectX0 r) (rectX1 r), max (rectX0 r) (rectX1 r))

joinParaLines :: [Line] -> T.Text
joinParaLines [] = T.empty
joinParaLines ls =
  T.strip $ foldl1 mergeText (map (T.strip . lineText) ls)
  where
    mergeText a b =
      let a' = T.stripEnd a
          b' = T.stripStart b
          sep =
            if not (T.null a') && not (T.null b')
               && isCJK (T.last a') && isCJK (T.head b')
            then T.empty
            else " "
      in a' `T.append` sep `T.append` b'

terminalChars :: String
terminalChars = "。．！？!?…"

closingChars :: String
closingChars = "」』）)]】〉》\"'"

endsWithTerminal :: T.Text -> Bool
endsWithTerminal t = endsWithTerminal' (T.strip t)
  where
    endsWithTerminal' s
      | T.null s = False
      | otherwise =
          case T.unsnoc s of
            Nothing -> False
            Just (init, c)
              | c `elem` closingChars -> endsWithTerminal' init
              | c `elem` terminalChars -> True
              | otherwise -> False
