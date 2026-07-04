#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring, directory, filepath
-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generate small, redistributable PDF fixtures into data/fixtures/.
--
-- These are authored from scratch (no third-party content), so unlike
-- data/sample they can live in the public repository and run in CI.
--
-- Fixtures:
--
--   * classic.pdf     - traditional xref table
--   * xrefstream.pdf  - PDF 1.5 cross-reference stream (uncompressed)
--   * incremental.pdf - classic body plus one incremental update (/Prev chain)
--
-- Usage: cabal run scripts/gen_fixtures.hs [-- OUTDIR]
-- (OUTDIR defaults to data/fixtures, relative to the current directory)

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (sortOn)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.Printf (printf)

obj :: Int -> BS.ByteString -> BS.ByteString
obj n body = BS.concat [BS.pack (show n), " 0 obj\n", body, "\nendobj\n"]

contentStream :: BS.ByteString -> BS.ByteString
contentStream text =
  let strm = BS.concat ["BT /F1 24 Tf 72 720 Td (", text, ") Tj ET"]
  in BS.concat
       [ "<< /Length ", BS.pack (show (BS.length strm)), " >>\nstream\n"
       , strm
       , "\nendstream" ]

baseObjects :: BS.ByteString -> [(Int, BS.ByteString)]
baseObjects text =
  [ (1, "<< /Type /Catalog /Pages 2 0 R >>")
  , (2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
  , (3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] \
        \/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
  , (4, contentStream text)
  , (5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
  ]

-- | Serialize objects in ascending order, tracking each byte offset.
buildBody :: [(Int, BS.ByteString)] -> (BS.ByteString, [(Int, Int)])
buildBody objects = go header [] (sortOn fst objects)
  where
    header = "%PDF-1.5\n%\xc2\xb5\xc2\xb6\n"
    go acc offs [] = (acc, reverse offs)
    go acc offs ((n, b) : rest) =
      go (acc <> obj n b) ((n, BS.length acc) : offs) rest

xrefEntry :: Int -> BS.ByteString
xrefEntry off = BS.pack (printf "%010d 00000 n \n" off)

xrefTable :: [(Int, Int)] -> Int -> BS.ByteString
xrefTable offsets size = BS.concat $
  [ "xref\n"
  , BS.pack ("0 " ++ show size ++ "\n")
  , "0000000000 65535 f \n"
  ] ++ map (xrefEntry . snd) offsets

trailerPart :: Maybe Int -> Int -> BS.ByteString
trailerPart mprev xrefPos = BS.concat
  [ "trailer\n<< /Size 6 /Root 1 0 R"
  , maybe "" (\p -> BS.pack (" /Prev " ++ show p)) mprev
  , " >>\nstartxref\n"
  , BS.pack (show xrefPos)
  , "\n%%EOF\n" ]

classicWithPos :: (BS.ByteString, Int)
classicWithPos =
  let (body, offsets) = buildBody (baseObjects "Hello classic xref")
      xrefPos = BS.length body
      out = body <> xrefTable offsets 6 <> trailerPart Nothing xrefPos
  in (out, xrefPos)

classic :: BS.ByteString
classic = fst classicWithPos

-- | Two-byte big-endian field for the xref stream.
be2 :: Int -> BS.ByteString
be2 n = BS.pack [toEnum (n `div` 256), toEnum (n `mod` 256)]

xrefstream :: BS.ByteString
xrefstream =
  let (body, offsets) = buildBody (baseObjects "Hello xref stream")
      xrefPos = BS.length body
      -- W [1 2 2]: type(1) offset(2) generation(2), objects 0..6
      entries = BS.concat $
        [ "\x00\x00\x00\xff\xff" ]                                -- obj 0: free
        ++ [ "\x01" <> be2 off <> "\x00\x00" | (_, off) <- offsets ]
        ++ [ "\x01" <> be2 xrefPos <> "\x00\x00" ]                -- obj 6: this stream
      dictPart = BS.concat
        [ "<< /Type /XRef /Size 7 /W [1 2 2] /Root 1 0 R /Length "
        , BS.pack (show (BS.length entries)), " >>" ]
  in BS.concat
       [ body
       , "6 0 obj\n", dictPart, "\nstream\n", entries, "\nendstream\nendobj\n"
       , "startxref\n", BS.pack (show xrefPos), "\n%%EOF\n" ]

incremental :: BS.ByteString
incremental =
  let (base, firstXref) = classicWithPos
      updateOffset = BS.length base
      out1 = base <> obj 4 (contentStream "Hello incremental update")
      xrefPos = BS.length out1
  in BS.concat
       [ out1
       , "xref\n4 1\n", xrefEntry updateOffset
       , trailerPart (Just firstXref) xrefPos ]

main :: IO ()
main = do
  args <- getArgs
  let outdir = case args of
        (d : _) -> d
        []      -> "data" </> "fixtures"
  createDirectoryIfMissing True outdir
  mapM_ (write outdir)
    [ ("classic.pdf", classic)
    , ("xrefstream.pdf", xrefstream)
    , ("incremental.pdf", incremental)
    ]
  where
    write outdir (name, bytes) = do
      let path = outdir </> name
      BS.writeFile path bytes
      putStrLn $ "wrote " ++ path ++ " (" ++ show (BS.length bytes) ++ " bytes)"
