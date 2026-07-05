{-# LANGUAGE OverloadedStrings #-}

module PDF.Layout
  ( Rect(..)
  , PageItem(..)
  , Line(..)
  , LayoutOptions(..)
  , defaultLayoutOptions
  , needsAozoraBar
  , aozoraRuby
  , layoutParagraphs
  , layoutParagraphsWith
  , layoutPageText
  , layoutPageTextWith
  , layoutDocument
  , layoutDocumentWith
  , stripHeadersFooters
  , linesFromGlyphs
  , sortLinesByReadingOrder
  , joinParaLines
  , intraLineSpace
  , joinGlyphsRun
  , pageItemLines
  , pageItemParagraphGroups
  ) where

import PDF.Interpret (Glyph(..), Rect(..), PageItem(..))

import Data.Char (isDigit, isSpace, ord)
import Data.List (foldl', maximumBy, partition, sort, sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (Down(..), comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- Footnote bodies continuing onto the following page are not merged.
data LayoutOptions = LayoutOptions
  { optFootnotes :: Bool
  , optRuby      :: Bool
  } deriving (Eq, Show)

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions {optFootnotes = False, optRuby = False}

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
  case applyFootnotes opts (applyRuby opts (pageLines items)) of
    PageFallback ps -> ps
    PageNormal wmode graphics bounds ls ->
      map joinParaLines (groupParagraphs wmode graphics bounds ls)

pageItemLines :: LayoutOptions -> [PageItem] -> [Line]
pageItemLines opts items =
  case applyFootnotes opts (applyRuby opts (pageLines items)) of
    PageFallback _ -> []
    PageNormal _ _ _ ls -> ls

pageItemParagraphGroups :: LayoutOptions -> [PageItem] -> [[Line]]
pageItemParagraphGroups opts items =
  case applyFootnotes opts (applyRuby opts (pageLines items)) of
    PageFallback ps -> replicate (length ps) []
    PageNormal wmode graphics bounds ls ->
      groupParagraphs wmode graphics bounds ls

documentParagraphs :: LayoutOptions -> [[PageItem]] -> [T.Text]
documentParagraphs opts pages =
  let pageCount = length pages
      layouts = map pageLines pages
      stripped = applyHeaderFooterStrip pageCount layouts
      final = map (applyFootnotes opts . applyRuby opts) stripped
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

applyRuby :: LayoutOptions -> PageLines -> PageLines
applyRuby opts page =
  case page of
    PageNormal wmode _ bounds ls ->
      PageNormal wmode [] bounds (mergeInterleavedRubyLines wmode (optRuby opts) ls)
    _ -> page

-- Aozora bunko ruby: fullwidth 《》 and optional ｜ before mixed-script bases.
aozoraRuby :: T.Text -> T.Text -> T.Text
aozoraRuby base ruby =
  let prefix = if needsAozoraBar base then "\65372" else T.empty  -- ｜
  in base `T.append` prefix `T.append` "\12298" `T.append` ruby `T.append` "\12299"  -- 《》

needsAozoraBar :: T.Text -> Bool
needsAozoraBar t =
  let cats = S.fromList (mapMaybe scriptCategory (T.unpack t))
  in S.size cats >= 2

data ScriptCat = CatHiragana | CatKatakana | CatCJK | CatLatin | CatOther
  deriving (Eq, Ord, Show)

scriptCategory :: Char -> Maybe ScriptCat
scriptCategory c =
  let cp = ord c
  in if cp >= 0x3041 && cp <= 0x309F
     then Just CatHiragana
     else if cp >= 0x30A1 && cp <= 0x30FF
          then Just CatKatakana
          else if (cp >= 0x4E00 && cp <= 0x9FFF)
               || (cp >= 0x3400 && cp <= 0x4DBF)
               || (cp >= 0xF900 && cp <= 0xFAFF)
               then Just CatCJK
               else if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
                    then Just CatLatin
                    else if isSpace c
                         then Nothing
                         else Just CatOther

pairRubyLines :: Int -> (Double, Double) -> [Line] -> [Line]
pairRubyLines wmode _ = mergeInterleavedRubyLines wmode True

isRubyLine :: Int -> Double -> [Line] -> Line -> Bool
isRubyLine wmode bodySize ls l =
  lineSize l <= 0.85 * bodySize
  && not (T.null (T.strip (lineText l)))
  && any (rubyAlignsWithParent wmode l) (filter isBodyLine ls)
  where
    isBodyLine b = lineSize b > 0.85 * bodySize

bodyMedianSize :: [Line] -> Double
bodyMedianSize ls =
  let sizes = map lineSize ls
      med = medianOf sizes
      bodySizes = [lineSize l | l <- ls, lineSize l > 0.85 * med]
  in if null bodySizes then med else medianOf bodySizes

baselineClose :: Double -> Line -> Line -> Bool
baselineClose bodySize a b =
  abs (lineBaseline a - lineBaseline b) <= 0.4 * bodySize

shortBodyLine :: Line -> Bool
shortBodyLine l = T.length (T.strip (lineText l)) <= 2

firstCharText :: T.Text -> T.Text
firstCharText t =
  case T.uncons (T.strip t) of
    Nothing -> T.empty
    Just (c, _) -> T.singleton c

clusterBaseText :: [(Line, Line)] -> T.Text
clusterBaseText pairs =
  let bs = map fst pairs
  in T.concat
       [ if length bs == 1
         then T.strip (lineText b)
         else if i == length bs - 1 && not (shortBodyLine b)
              then firstCharText (lineText b)
              else T.strip (lineText b)
       | (i, b) <- zip [0 ..] bs
       ]

clusterSuffixText :: [(Line, Line)] -> T.Text
clusterSuffixText pairs =
  case reverse (map fst pairs) of
    b : _ | not (shortBodyLine b) ->
      let t = T.strip (lineText b)
      in if T.null t then T.empty else T.drop 1 t
    _ -> T.empty

clusterRubyText :: [(Line, Line)] -> T.Text
clusterRubyText = T.concat . map (T.strip . lineText . snd)

clusterContinuation :: Line -> Line -> Bool
clusterContinuation prev cur =
  baselineClose (lineSize cur) prev cur
  && lineInlineStart cur - lineInlineEnd prev <= 2 * lineSize cur

removeRubyLine :: Line -> [Line] -> [Line]
removeRubyLine r = filter (not . sameRubyLine r)

sameRubyLine :: Line -> Line -> Bool
sameRubyLine a b =
  lineBaseline a == lineBaseline b
  && lineInlineStart a == lineInlineStart b
  && lineText a == lineText b

findRubyForBody :: Int -> [Line] -> Line -> Maybe Line
findRubyForBody wmode rubyLs body =
  case [ r
       | r <- rubyLs
       , rubyAlignsWithParent wmode r body
       ] of
    [] -> Nothing
    rs -> Just (maximumBy (comparing (\r -> rubyOverlapFrac wmode r body)) rs)

mergeInterleavedRubyLines :: Int -> Bool -> [Line] -> [Line]
mergeInterleavedRubyLines wmode includeRuby ls
  | null ls = ls
  | otherwise =
      let bodySize = bodyMedianSize ls
          (rubyLs, bodyLs) = partition (isRubyLine wmode bodySize ls) ls
      in if null rubyLs
         then ls
         else mergeBodyBands wmode includeRuby bodySize rubyLs bodyLs

mergeBodyBands :: Int -> Bool -> Double -> [Line] -> [Line] -> [Line]
mergeBodyBands wmode includeRuby bodySize rubyLs bodyLs =
  let bands = groupBodyBaselineBands bodySize (sortLinesByReadingOrder bodyLs)
  in concatMap (mergeOneBand wmode includeRuby bodySize rubyLs) bands

groupBodyBaselineBands :: Double -> [Line] -> [[Line]]
groupBodyBaselineBands _ [] = []
groupBodyBaselineBands bodySize (l:ls) =
  let (same, rest) = span (baselineClose bodySize l) ls
  in (l : same) : groupBodyBaselineBands bodySize rest

mergeOneBand :: Int -> Bool -> Double -> [Line] -> [Line] -> [Line]
mergeOneBand wmode includeRuby bodySize allRuby bodyBand =
  let sorted = sortBy (comparing lineInlineStart) bodyBand
      (segments, _) = foldSegments wmode bodySize allRuby sorted
      repStart = head sorted
      repEnd = last sorted
      txt = renderRubySegments includeRuby segments
  in if T.null txt
     then []
     else [ repStart
              { lineText = txt
              , lineInlineEnd = lineInlineEnd repEnd
              , lineInlineStart = lineInlineStart repStart
              , lineSize = max (lineSize repStart) (lineSize repEnd)
              }
          ]

data RubySegment
  = PlainSeg !Line
  | ClusterSeg [(Line, Line)]
  deriving (Show)

foldSegments :: Int -> Double -> [Line] -> [Line] -> ([RubySegment], [Line])
foldSegments wmode bodySize rubyPool =
  go rubyPool
  where
    go pool [] = ([], pool)
    go pool (b:bs) =
      case findRubyForBody wmode pool b of
        Nothing ->
          let (plain, rest) = span (\l' -> case findRubyForBody wmode pool l' of Nothing -> True; _ -> False) (b:bs)
              (rest', pool') = go pool (drop (length plain) (b:bs))
          in (map PlainSeg plain ++ rest', pool')
        Just r ->
          let (cluster, restBs, pool') = spanCluster wmode bodySize (removeRubyLine r pool) r b bs
              (rest', pool'') = go pool' restBs
          in (ClusterSeg cluster : rest', pool'')

spanCluster :: Int -> Double -> [Line] -> Line -> Line -> [Line]
           -> ([(Line, Line)], [Line], [Line])
spanCluster wmode bodySize pool r b bs =
  go pool [(b, r)] bs
  where
    go rp pairs (b':bs') =
      case findRubyForBody wmode rp b' of
        Just r' | clusterContinuation (fst (last pairs)) b' ->
          go (removeRubyLine r' rp) ((b', r') : pairs) bs'
        _ -> (reverse pairs, b' : bs', rp)
    go rp pairs [] = (reverse pairs, [], rp)

renderRubySegments :: Bool -> [RubySegment] -> T.Text
renderRubySegments includeRuby =
  T.concat . map renderSeg
  where
    renderSeg (PlainSeg l) = T.strip (lineText l)
    renderSeg (ClusterSeg pairs) =
      let base = clusterBaseText pairs
          ruby = clusterRubyText pairs
          suffix = clusterSuffixText pairs
          marked =
            if includeRuby
            then aozoraRuby base ruby
            else base
      in marked `T.append` suffix

rubyAlignsWithParent :: Int -> Line -> Line -> Bool
rubyAlignsWithParent wmode ruby parent =
  let bodySize = lineSize parent
      offset = rubyOffset wmode parent ruby
      overlap = rubyOverlapFrac wmode ruby parent
  in lineSize ruby <= 0.85 * bodySize
     && offset > 0.15 * bodySize && offset <= 1.2 * bodySize
     && overlap >= 0.2

rubyOverlapFrac :: Int -> Line -> Line -> Double
rubyOverlapFrac _ ruby parent =
  let rLo = min (lineInlineStart ruby) (lineInlineEnd ruby)
      rHi = max (lineInlineStart ruby) (lineInlineEnd ruby)
      bLo = min (lineInlineStart parent) (lineInlineEnd parent)
      bHi = max (lineInlineStart parent) (lineInlineEnd parent)
      lo = max rLo bLo
      hi = min rHi bHi
      overlap = max 0 (hi - lo)
      span = max (rHi - rLo) 1
  in overlap / span

rubyOffset :: Int -> Line -> Line -> Double
rubyOffset wmode parent ruby =
  if wmode == 1
  then lineBaseline parent - lineBaseline ruby
  else lineBaseline ruby - lineBaseline parent

inlineOverlapFrac :: Int -> Line -> Line -> Double
inlineOverlapFrac _ a b =
  let aLo = min (lineInlineStart a) (lineInlineEnd a)
      aHi = max (lineInlineStart a) (lineInlineEnd a)
      bLo = min (lineInlineStart b) (lineInlineEnd b)
      bHi = max (lineInlineStart b) (lineInlineEnd b)
      lo = max aLo bLo
      hi = min aHi bHi
      overlap = max 0 (hi - lo)
      span = max (bHi - bLo) 1
  in overlap / span

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
      | otherwise =
          let sep = paraJoinSep (T.stripEnd a) (T.stripStart b)
          in if T.null sep
             then a `T.append` b
             else a `T.append` sep `T.append` b

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
          else let visible = filterPageGlyphs glyphs
                   wmode = dominantWMode visible
                   pageBounds = pageExtents visible
                   ls = map fixDingbatBulletLine (buildLines visible)
               in PageNormal wmode graphics pageBounds ls

linesFromGlyphs :: [Glyph] -> [Line]
linesFromGlyphs = buildLines

-- RTL horizontal text is not supported; only LTR horizontal and vertical CJK.
sortLinesByReadingOrder :: [Line] -> [Line]
sortLinesByReadingOrder [] = []
sortLinesByReadingOrder ls =
  let (w0, w1) = partition ((== 0) . lineWMode) ls
      sortHoriz = sortOn (\l -> (Down (lineBaseline l), lineFirstInline l))
      sortVert = sortOn (\l -> (Down (lineBaseline l), Down (lineFirstInline l)))
  in if null w0 || null w1
     then if null w1 then sortHoriz w0 else sortVert w1
     else sortHoriz w0 ++ sortVert w1
  where
    sortOn f = sortBy (comparing f)

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

-- | Drop coordinate outliers (e.g. footnote digits emitted far off-page).
filterPageGlyphs :: [Glyph] -> [Glyph]
filterPageGlyphs glyphs =
  let horiz = filter ((== 0) . glyphWMode) glyphs
      vert = filter ((== 1) . glyphWMode) glyphs
      horizVis = filter ((>= 0) . glyphY) horiz
  in filter (\g -> glyphInBand g horizVis vert) glyphs
  where
    glyphInBand g hs vs
      | glyphWMode g == 1 = inBand (baselineOf 1) vs g
      | otherwise = inBand glyphY hs g

    inBand measure [] g = measure g >= 0
    inBand measure gs g =
      let v = measure g
      in v >= 0 && case baselineBand measure gs of
           Nothing -> True
           Just (lo, hi) -> v >= lo && v <= hi

    baselineBand measure gs =
      let ys = sort (map measure gs)
      in if length ys < 4
         then Nothing
         else let q1 = quantile 0.25 ys
                  q3 = quantile 0.75 ys
                  iqr = q3 - q1
                  medSize = medianOf (map glyphSize gs)
                  spread = max (max 1 iqr) (1.2 * medSize)
                  pad = 3 * spread
              in Just (q1 - pad, q3 + pad)

    quantile q xs =
      let n = length xs
          i = min (n - 1) (max 0 (truncate (q * fromIntegral (n - 1))))
      in xs !! i

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
        inlineCont refSize = gap >= -0.5 * refSize && gap <= 2.0 * refSize
        superAttach =
          glyphSize g <= 0.92 * lineSize l
          && glyphSize g >= 0.5 * lineSize l
          && inlineCont (lineSize l)
          && ((d > 0.25 * lineSize l && d <= 0.75 * lineSize l)
              || ((-d) > 0.25 * lineSize l && (-d) <= 0.4 * lineSize l))
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
  | mc == Just '-' || nc == Just '-' = ""
  | latinAdjacent mc nc, gap >= 0.25 * size = " "
  | gap > 2.0 * size = " "
  | gap > 0.3 * size, not (cjkAdjacent mc nc) = " "
  | otherwise = ""

isLatinLetter :: Char -> Bool
isLatinLetter c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

latinAdjacent :: Maybe Char -> Maybe Char -> Bool
latinAdjacent (Just a) (Just b) =
  not (isCJK a || isCJK b) && (isLatinLetter a || isLatinLetter b)
latinAdjacent _ _ = False

hyphenContinues :: Char -> Bool
hyphenContinues c = c == '-' || c == '\x00AD'

paraJoinSep :: T.Text -> T.Text -> T.Text
paraJoinSep a' b'
  | not (T.null a') && not (T.null b')
    && isCJK (T.last a') && isCJK (T.head b') = T.empty
  | not (T.null a') && hyphenContinues (T.last a') = T.empty
  | otherwise = " "

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

listMarkerStart :: Line -> Bool
listMarkerStart l =
  let t = T.stripStart (lineText l)
  in letteredMarker t || numberedListMarker t
  where
    letteredMarker t =
      case T.uncons t of
        Just (c, rest) | c >= 'a' && c <= 'z' ->
          case T.uncons (T.stripStart rest) of
            Just ('.', _) -> True
            _ -> False
        _ -> False
    numberedListMarker t =
      case T.uncons t of
        Just (c, rest) | isDigit c ->
          case T.span isDigit t of
            (ds, rest') ->
              not (T.null ds)
              && T.length ds <= 2
              && case T.uncons (T.stripStart rest') of
                   Just ('.', _) -> True
                   _ -> False
        _ -> False

hangWrappedContinuation :: Line -> Line -> Bool
hangWrappedContinuation prev cur =
  lineFirstInline cur > lineFirstInline prev + 0.6 * lineSize prev

afterListHeadingBreak :: Int -> Line -> Line -> [Double] -> Bool
afterListHeadingBreak wmode prev cur gaps =
  listMarkerStart prev
  && not (hangWrappedContinuation prev cur)
  && abs (baselineGap wmode prev cur)
       >= 0.75 * typicalLeading gaps (lineSize cur)

listItemEnd :: Line -> Bool
listItemEnd l =
  let t = T.strip (lineText l)
  in T.isSuffixOf "こと" t || endsWithTerminal t

sameHangListItemBreak :: Int -> Line -> Line -> [Double] -> Bool
sameHangListItemBreak wmode prev cur gaps
  | isCodeLine prev || isCodeLine cur = False
  | not (listItemEnd prev) = False
  | otherwise =
      let gap = abs (baselineGap wmode prev cur)
          typical = typicalLeading gaps (lineSize cur)
          tol = 0.35 * lineSize cur
      in cjkAdjacent (lastChar (lineText prev)) (firstChar (lineText cur))
         && abs (lineFirstInline cur - lineFirstInline prev) <= tol
         && gap >= 0.85 * typical
         && not (hangWrappedContinuation prev cur)

isNumberedCodeLine :: Line -> Bool
isNumberedCodeLine l = numberedCodeStart (T.stripStart (lineText l))

numberedCodeStart :: T.Text -> Bool
numberedCodeStart t =
  case T.uncons t of
    Just (c, rest) | isDigit c ->
      case T.span isDigit t of
        (ds, rest') ->
          not (T.null ds)
          && case T.uncons (T.stripStart rest') of
               Just (codeSep, _)
                 | codeSep == ' ' || codeSep == '.' -> True
               _ -> False
    _ -> False

isCodeLine :: Line -> Bool
isCodeLine l =
  isNumberedCodeLine l
  || (lineSize l <= 7.5 && smallMonospaceLine l && highLatinFraction (lineText l))

smallMonospaceLine :: Line -> Bool
smallMonospaceLine l =
  let t = T.strip (lineText l)
  in not (T.null t)
     && lineSize l > 0
     && T.any isLatinLetter t
     && not (T.any isCJK t)

highLatinFraction :: T.Text -> Bool
highLatinFraction t =
  let chars = filter (not . isSpace) (T.unpack t)
      latin = length (filter isLatinLetter chars)
  in not (null chars) && fromIntegral latin / fromIntegral (length chars) >= 0.5

codeBlockBreak :: Line -> Line -> Bool
codeBlockBreak prev cur =
  isCodeLine cur /= isCodeLine prev

codeIndentSpaces :: Double -> Double -> Line -> T.Text
codeIndentSpaces minX charW l =
  let offset = max 0 (lineFirstInline l - minX)
      n = truncate (offset / max charW 1)
  in T.replicate n " "

joinCodeLines :: [Line] -> T.Text
joinCodeLines ls =
  let minX = minimum (map lineFirstInline ls)
      charW = minimum (map (max 1 . (* 0.55) . lineSize) ls)
  in T.intercalate "\n"
       [ codeIndentSpaces minX charW l `T.append` T.strip (lineText l)
       | l <- ls
       ]

groupParagraphs :: Int -> [Rect] -> (Double, Double) -> [Line] -> [[Line]]
groupParagraphs wmode graphics bounds lines =
  go [] (filter (not . T.null . T.strip . lineText) (sortLinesByReadingOrder lines))
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
      gapBreak = abs gap > 1.6 * typical
  in negativeAdvance wmode prev cur
  || listMarkerStart cur
  || afterListHeadingBreak wmode prev cur gaps
  || sameHangListItemBreak wmode prev cur gaps
  || codeBlockBreak prev cur
  || (gapBreak && not (cjkWrapContinuation prev cur))
  || indentBreak paraMinInline cur
  || (graphicBreak wmode graphics pageBounds prev cur
      && not (cjkWrapContinuation prev cur))

cjkWrapContinuation :: Line -> Line -> Bool
cjkWrapContinuation prev cur =
  case (lastChar (lineText prev), firstChar (lineText cur)) of
    (Just a, Just b) ->
      isCJK a && isCJK b && not (endsWithTerminal (lineText prev))
    _ -> False

fixDingbatBulletLine :: Line -> Line
fixDingbatBulletLine l = l {lineText = fixDingbatBullet (lineText l)}

fixDingbatBullet :: T.Text -> T.Text
fixDingbatBullet t =
  let open = T.singleton '\x300c'
      t1 = fixPrefix t
  in T.replace (T.pack " r" `T.append` open) (T.pack " \8226" `T.append` open) t1
  where
    fixPrefix t =
      case T.uncons t of
        Just ('r', rest) ->
          case T.uncons rest of
            Just ('\x300c', _) -> T.cons '\x2022' rest
            Just (' ', rest') ->
              case T.uncons rest' of
                Just (c, _) | not (isLowerLatin c) -> T.cons '\x2022' (T.cons ' ' rest')
                _ -> t
            Nothing -> T.singleton '\x2022'
            _ -> t
        _ -> t
    isLowerLatin c = c >= 'a' && c <= 'z'

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
joinParaLines ls
  | all isCodeLine ls = joinCodeLines ls
  | otherwise =
      T.strip $ foldl1 mergeText (map (T.strip . lineText) ls)
  where
    mergeText a b =
      let a' = T.stripEnd a
          b' = T.stripStart b
      in a' `T.append` paraJoinSep a' b' `T.append` b'

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
