module TuiGeometry
  ( HeightSpec(..)
  , parseHeightSpec
  , viewportHeight
  ) where

import Data.List (isSuffixOf)

data HeightSpec = HeightRows Int | HeightPercent Int
  deriving (Eq, Show)

parseHeightSpec :: String -> Maybe HeightSpec
parseHeightSpec s
  | null s = Nothing
  | "%" `isSuffixOf` s =
      case reads (take (length s - 1) s) of
        [(n, "")] | n >= 0 && n <= 100 -> Just (HeightPercent n)
        _ -> Nothing
  | otherwise =
      case reads s of
        [(n, "")] | n > 0 -> Just (HeightRows n)
        _ -> Nothing

viewportHeight :: Int -> Maybe HeightSpec -> Int
viewportHeight termRows mSpec =
  let raw = case mSpec of
        Nothing -> termRows `div` 2
        Just (HeightRows n) -> n
        Just (HeightPercent p) -> termRows * p `div` 100
  in max 6 (min termRows raw)
