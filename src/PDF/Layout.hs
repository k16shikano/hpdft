{-# LANGUAGE OverloadedStrings #-}

module PDF.Layout
  ( Rect(..)
  , PageItem(..)
  , layoutParagraphs
  , layoutPageText
  , intraLineSpace
  , joinGlyphsRun
  ) where

import PDF.Interpret (Glyph(..), Rect(..), PageItem(..))

import Data.Char (ord)
import Data.List (foldl', maximumBy, sort)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Text as T

layoutPageText :: [PageItem] -> T.Text
layoutPageText items =
  let ps = layoutParagraphs items
  in if null ps
     then "\n"
     else T.intercalate "\n\n" ps `T.append` "\n"

layoutParagraphs :: [PageItem] -> [T.Text]
layoutParagraphs items =
  let glyphs = [g | ItemGlyph g <- items]
      graphics = [r | ItemGraphic r <- items]
  in if null glyphs
     then []
     else if fallbackNeeded glyphs
          then [fallbackText glyphs]
          else let wmode = dominantWMode glyphs
                   pageBounds = pageExtents glyphs
                   ls = buildLines glyphs
               in map joinParaLines (groupParagraphs wmode graphics pageBounds ls)

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
  } deriving (Show)

buildLines :: [Glyph] -> [Line]
buildLines = reverse . foldl' go []
  where
    go [] g = [newLine g]
    go (l:ls) g
      | glyphWMode g /= lineWMode l = newLine g : l : ls
      | abs (baselineOf (lineWMode l) g - lineBaseline l)
          <= 0.4 * max (glyphSize g) (lineSize l) =
          mergeGlyph l g : ls
      | otherwise = newLine g : l : ls

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
  lineFirstInline cur - paraMinInline > 1.0 * lineSize cur

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
