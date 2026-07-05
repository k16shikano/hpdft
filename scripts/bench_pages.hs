{-# LANGUAGE OverloadedStrings #-}
-- In-process benchmark: per-page interpret+layout cost on a PDF.
-- Usage: cabal exec -- ghc -O2 -package hpdft scripts/bench_pages.hs -o /tmp/bench_pages
--        /tmp/bench_pages data/sample/book.pdf
import PDF.Document (openDocument)
import PDF.Page (pageRefs)
import PDF.Interpret (interpretPageItems)
import PDF.Layout (defaultLayoutOptions, layoutPageTextWith)

import qualified Data.Text as T
import Control.Monad (forM_)
import System.Environment (getArgs)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

timeIt :: IO a -> IO (Double, a)
timeIt io = do
  t0 <- getCurrentTime
  x <- io
  t1 <- getCurrentTime
  return (realToFrac (diffUTCTime t1 t0), x)

forcePage :: (Int -> Either e [item]) -> ([item] -> T.Text) -> Int -> IO Int
forcePage interp layout ref =
  case interp ref of
    Right items -> let n = T.length (layout items) in n `seq` return n
    Left _ -> return 0

main :: IO ()
main = do
  [path] <- getArgs
  doc <- openDocument path Nothing >>= either (error . show) return
  refs <- either (error . show) return (pageRefs doc)
  printf "pages: %d\n" (length refs)
  let interp = interpretPageItems doc
      layout = layoutPageTextWith defaultLayoutOptions
  -- Same heavy page repeatedly: if iterations 2+ stay slow, per-page cost
  -- is recomputed every call (no font/CMap caching across calls).
  let heavy = refs !! min 49 (length refs - 1)
  forM_ [1 .. 5 :: Int] $ \i -> do
    (t, n) <- timeIt (forcePage interp layout heavy)
    printf "page50 iter %d: %.3fs (%d chars)\n" i t n
  (ttot, _) <- timeIt $
    forM_ (zip [1 :: Int ..] refs) $ \(i, r) -> do
      (t, _) <- timeIt (forcePage interp layout r)
      if i `mod` 25 == 0 then printf "  page %3d: %.3fs\n" i t else return ()
  printf "sum of all pages (interpret+layout, per page): %.3fs\n" ttot
