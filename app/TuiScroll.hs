module TuiScroll
  ( ScrollState(..)
  , initialScrollState
  , clampScrollTop
  , scrollBy
  , scrollToTop
  , scrollToEnd
  , scrollHalfPageDown
  , scrollHalfPageUp
  , searchForwardFrom
  , searchBackwardFrom
  , visibleLineRange
  , statusLineNumber
  , charDisplayWidth
  , stringDisplayWidth
  , clipToDisplayWidth
  , padToDisplayWidth
  ) where

import Data.Char (ord)
import Data.List (findIndex)

data ScrollState = ScrollState
  { scrollTopLine  :: !Int
  , scrollTotalLines :: !Int
  , scrollTextRows :: !Int
  } deriving (Eq, Show)

initialScrollState :: Int -> ScrollState
initialScrollState textRows =
  ScrollState {scrollTopLine = 0, scrollTotalLines = 0, scrollTextRows = textRows}

maxScrollTop :: ScrollState -> Int
maxScrollTop st =
  max 0 (scrollTotalLines st - scrollTextRows st)

clampScrollTop :: Int -> ScrollState -> ScrollState
clampScrollTop n st = st {scrollTopLine = clamp 0 (maxScrollTop st) n}
  where
    clamp lo hi x = max lo (min hi x)

scrollBy :: Int -> ScrollState -> ScrollState
scrollBy delta st =
  clampScrollTop (scrollTopLine st + delta) st

scrollToTop :: ScrollState -> ScrollState
scrollToTop st = st {scrollTopLine = 0}

scrollToEnd :: ScrollState -> ScrollState
scrollToEnd st = clampScrollTop maxBound st

scrollHalfPageDown :: ScrollState -> ScrollState
scrollHalfPageDown st =
  scrollBy (max 1 (scrollTextRows st `div` 2)) st

scrollHalfPageUp :: ScrollState -> ScrollState
scrollHalfPageUp st =
  scrollBy (-max 1 (scrollTextRows st `div` 2)) st

-- | Index of the first line at or after @start@ satisfying the predicate.
searchForwardFrom :: (a -> Bool) -> Int -> [a] -> Maybe Int
searchForwardFrom p start ls =
  let from = max 0 start
   in (+ from) <$> findIndex p (drop from ls)

-- | Index of the last line at or before @start@ satisfying the predicate.
searchBackwardFrom :: (a -> Bool) -> Int -> [a] -> Maybe Int
searchBackwardFrom p start ls =
  let upto = min (length ls - 1) start
      hits = [i | (i, l) <- zip [0 ..] (take (upto + 1) ls), p l]
   in if upto < 0 || null hits then Nothing else Just (last hits)

visibleLineRange :: ScrollState -> (Int, Int)
visibleLineRange st =
  let top = scrollTopLine st
      end = min (scrollTotalLines st) (top + scrollTextRows st)
   in (top, end)

statusLineNumber :: ScrollState -> Int
statusLineNumber st =
  if scrollTotalLines st == 0
    then 0
    else scrollTopLine st + 1

-- | Terminal display width: East Asian wide/fullwidth characters occupy
-- two columns. Covers the common CJK blocks; combining marks are not handled.
charDisplayWidth :: Char -> Int
charDisplayWidth c
  | w >= 0x1100 && w <= 0x115F = 2   -- Hangul Jamo
  | w >= 0x2E80 && w <= 0xA4CF = 2   -- CJK radicals .. Yi
  | w >= 0xAC00 && w <= 0xD7A3 = 2   -- Hangul syllables
  | w >= 0xF900 && w <= 0xFAFF = 2   -- CJK compatibility ideographs
  | w >= 0xFE30 && w <= 0xFE4F = 2   -- CJK compatibility forms
  | w >= 0xFF00 && w <= 0xFF60 = 2   -- fullwidth forms
  | w >= 0xFFE0 && w <= 0xFFE6 = 2
  | w >= 0x20000 && w <= 0x3FFFD = 2 -- CJK extensions
  | otherwise = 1
  where w = ord c

stringDisplayWidth :: String -> Int
stringDisplayWidth = sum . map charDisplayWidth

-- | Longest prefix that fits in @width@ display columns.
clipToDisplayWidth :: Int -> String -> String
clipToDisplayWidth width = go width
  where
    go _ [] = []
    go left (c : cs)
      | cw <= left = c : go (left - cw) cs
      | otherwise = []
      where cw = charDisplayWidth c

-- | Clip and right-pad with spaces to exactly @width@ display columns
-- (may fall one column short when a wide character straddles the edge).
padToDisplayWidth :: Int -> String -> String
padToDisplayWidth width s =
  let clipped = clipToDisplayWidth width s
   in clipped ++ replicate (width - stringDisplayWidth clipped) ' '
