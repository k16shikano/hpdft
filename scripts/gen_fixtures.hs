#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring, directory, filepath, zlib, cryptonite, memory
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Crypto.Hash (hash, MD5(..), Digest)
import Data.ByteArray (convert)
import Data.Bits (shiftR, xor, (.&.))
import Data.Char (chr)
import Data.List (sortOn, foldl')
import Data.Word (Word8, Word32)
import Codec.Compression.Zlib (compress)
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

contentStreamIndirectLength :: BS.ByteString -> Int -> BS.ByteString
contentStreamIndirectLength text lenObj =
  let strm = BS.concat ["BT /F1 24 Tf 72 720 Td (", text, ") Tj ET"]
  in BS.concat
       [ "<< /Length ", BS.pack (show lenObj), " 0 R >>\nstream\n"
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

pdfHeader :: BS.ByteString
pdfHeader = "%PDF-1.5\n%\xc2\xb5\xc2\xb6\n"

buildBody :: [(Int, BS.ByteString)] -> (BS.ByteString, [(Int, Int)])
buildBody objects = go pdfHeader [] (sortOn fst objects)
  where
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

xrefTableMulti :: [(Int, Int)] -> BS.ByteString
xrefTableMulti offsets =
  BS.concat $
    [ "xref\n"
    , "0 1\n"
    , "0000000000 65535 f \n"
    ] ++ map subsection (groupContiguous $ sortOn fst offsets)
  where
    subsection grp =
      BS.concat $
        BS.pack (show (fst (head grp)) ++ " " ++ show (length grp) ++ "\n")
        : map (xrefEntry . snd) grp

groupContiguous :: [(Int, Int)] -> [[(Int, Int)]]
groupContiguous [] = []
groupContiguous ovs =
  reverse $ foldl' go [] (sortOn fst ovs)
  where
    go [] x = [[x]]
    go (g : gs) x@(n, _)
      | n == fst (last g) + 1 = (g ++ [x]) : gs
      | otherwise = [x] : g : gs

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

be2 :: Int -> BS.ByteString
be2 n = BS.pack [toEnum (n `div` 256), toEnum (n `mod` 256)]

xrefstream :: BS.ByteString
xrefstream =
  let (body, offsets) = buildBody (baseObjects "Hello xref stream")
      xrefPos = BS.length body
      entries = BS.concat $
        [ "\x00\x00\x00\xff\xff" ]
        ++ [ "\x01" <> be2 off <> "\x00\x00" | (_, off) <- offsets ]
        ++ [ "\x01" <> be2 xrefPos <> "\x00\x00" ]
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

objStmObjects :: [(Int, BS.ByteString)]
objStmObjects =
  [ (1, "<< /Type /Catalog /Pages 2 0 R >>")
  , (2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
  , (3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] \
        \/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
  , (5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
  ]

objStmHeaderAndBody :: [(Int, BS.ByteString)] -> (BS.ByteString, BS.ByteString)
objStmHeaderAndBody objects =
  let bodies = BS.intercalate "\n" [b | (_, b) <- objects]
      nums = map fst objects
      bodyLens = map BS.length [b | (_, b) <- objects]
      offsRel = [ sum (take i bodyLens) + i | i <- [0 .. length objects - 1] ]
      fixHdr hdrLen =
        let hdr = BS.concat
              [ BS.pack (show n ++ " " ++ show o ++ " ") | (n, o) <- zip nums offsRel ]
            newLen = BS.length hdr
        in if newLen == hdrLen then (hdr, bodies) else fixHdr newLen
  in fixHdr 0



objstm :: BS.ByteString
objstm =
  let text = "Hello object stream"
      contentObj = (4, contentStream text)
      stmObjects = objStmObjects
      (hdr, bodies) = objStmHeaderAndBody stmObjects
      rawStm = hdr <> bodies
      compressed = BSL.toStrict $ compress (BSL.fromStrict rawStm)
      objStmBody = BS.concat
        [ "<< /Type /ObjStm /N ", BS.pack (show (length stmObjects))
        , " /First ", BS.pack (show (BS.length hdr))
        , " /Length ", BS.pack (show (BS.length compressed))
        , " /Filter /FlateDecode >>\nstream\n"
        , compressed
        , "\nendstream" ]
      bodyParts =
        [ contentObj
        , (7, objStmBody)
        ]
      (body, offsets) = buildBody bodyParts
      off4 = maybe 0 id $ lookup 4 offsets
      off7 = maybe 0 id $ lookup 7 offsets
      xrefPos = BS.length body
      type2 objNum idx = BS.concat ["\x02", be2 7, be2 idx]
      entries = BS.concat
        [ "\x00\x00\x00\xff\xff"
        , type2 1 0
        , type2 2 1
        , type2 3 2
        , "\x01" <> be2 off4 <> "\x00\x00"
        , type2 5 3
        , "\x01" <> be2 xrefPos <> "\x00\x00"
        , "\x01" <> be2 off7 <> "\x00\x00"
        ]
      xrefDict = BS.concat
        [ "<< /Type /XRef /Size 8 /W [1 2 2] /Root 1 0 R /Length "
        , BS.pack (show (BS.length entries)), " >>" ]
  in BS.concat
       [ body
       , "6 0 obj\n", xrefDict, "\nstream\n", entries, "\nendstream\nendobj\n"
       , "startxref\n", BS.pack (show xrefPos), "\n%%EOF\n" ]

indirectLength :: BS.ByteString
indirectLength =
  let text = "Hello indirect length"
      strm = BS.concat ["BT /F1 24 Tf 72 720 Td (", text, ") Tj ET"]
      lenVal = BS.length strm
      objects =
        [ (1, "<< /Type /Catalog /Pages 2 0 R >>")
        , (2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
        , (3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] \
              \/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
        , (8, BS.pack (show lenVal))
        , (4, contentStreamIndirectLength text 8)
        , (5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
        ]
      (body, offsets) = buildBody objects
      xrefPos = BS.length body
      out = body <> xrefTableMulti offsets <> trailerPartSize 9 Nothing xrefPos
  in out

trailerPartSize :: Int -> Maybe Int -> Int -> BS.ByteString
trailerPartSize size mprev xrefPos = BS.concat
  [ "trailer\n<< /Size ", BS.pack (show size), " /Root 1 0 R"
  , maybe "" (\p -> BS.pack (" /Prev " ++ show p)) mprev
  , " >>\nstartxref\n"
  , BS.pack (show xrefPos)
  , "\n%%EOF\n" ]

binaryEndstream :: BS.ByteString
binaryEndstream =
  let decoyData = BS.concat
        [ "binary\xff\xfe data with literal "
        , "endstream"
        , " in the middle and also "
        , "endobj"
        , " token here"
        ]
      decoyObj = BS.concat
        [ "<< /Length ", BS.pack (show (BS.length decoyData)), " >>\nstream\n"
        , decoyData
        , "\nendstream" ]
      objects =
        [ (1, "<< /Type /Catalog /Pages 2 0 R >>")
        , (2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
        , (3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] \
              \/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
        , (6, decoyObj)
        , (4, contentStream "Hello binary stream")
        , (5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
        ]
      (body, offsets) = buildBody objects
      xrefPos = BS.length body
      out = body <> xrefTable offsets 7 <> trailerPartSize 7 Nothing xrefPos
  in out

padString :: BS.ByteString
padString = BS.pack
  [ '\x28', '\xBF', '\x4E', '\x5E', '\x4E', '\x75', '\x8A', '\x41'
  , '\x64', '\x00', '\x4E', '\x56', '\xFF', '\xFA', '\x01', '\x08'
  , '\x2E', '\x2E', '\x00', '\xB6', '\xD0', '\x68', '\x3E', '\x80'
  , '\x2F', '\x0C', '\xA9', '\xFE', '\x64', '\x53', '\x69', '\x7A'
  ]

md5 :: BS.ByteString -> BS.ByteString
md5 bs = convert (hash bs :: Digest MD5)

padPassword :: BS.ByteString -> BS.ByteString
padPassword pw = BS.take 32 $ pw `BS.append` padString

int32LE :: Int -> BS.ByteString
int32LE n =
  let w = fromIntegral n :: Word32
  in BS.pack
  [ chr (fromIntegral (w .&. 0xff))
  , chr (fromIntegral ((w `shiftR` 8) .&. 0xff))
  , chr (fromIntegral ((w `shiftR` 16) .&. 0xff))
  , chr (fromIntegral ((w `shiftR` 24) .&. 0xff))
  ]

int24LE :: Int -> BS.ByteString
int24LE n =
  let w = fromIntegral n :: Word32
  in BS.pack
  [ chr (fromIntegral (w .&. 0xff))
  , chr (fromIntegral ((w `shiftR` 8) .&. 0xff))
  , chr (fromIntegral ((w `shiftR` 16) .&. 0xff))
  ]

int16LE :: Int -> BS.ByteString
int16LE n =
  let w = fromIntegral n :: Word32
  in BS.pack
  [ chr (fromIntegral (w .&. 0xff))
  , chr (fromIntegral ((w `shiftR` 8) .&. 0xff))
  ]

rc4KeyStream :: BS.ByteString -> Int -> [Word8]
rc4KeyStream key nbytes =
  let keyInts = map fromEnum (BS.unpack key) :: [Int]
      state = ksaInit keyInts
  in map fromIntegral $ take nbytes $ prga state
  where
    ksaInit k' =
      let len = length k'
          step (st, j) i =
            let j' = (j + st !! i + k' !! (i `mod` len)) `mod` 256
                st' = swap st i j'
            in (st', j')
      in fst $ foldl' step ([0..255], 0) [0..255]
    prga st = reverse $ go st 0 0 []
      where go sbox i j out
              | length out >= nbytes = out
              | otherwise =
                  let i' = (i + 1) `mod` 256
                      j' = (j + sbox !! i') `mod` 256
                      s' = swap sbox i' j'
                      byte = s' !! ((s' !! i' + s' !! j') `mod` 256)
                  in go s' i' j' (byte:out)
    swap s i j =
      [ if x == i then s !! j else if x == j then s !! i else v
      | (x, v) <- zip [0..] s ]

rc4Encrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString
rc4Encrypt key plaintext =
  let ks = rc4KeyStream key (BS.length plaintext)
  in BS.pack $ zipWith (\c k -> chr (xor (fromEnum c) (fromIntegral k))) (BS.unpack plaintext) ks

computeOwnerO :: BS.ByteString -> Int -> BS.ByteString
computeOwnerO ownerPassword keyLen =
  let hashed = md5 (padPassword ownerPassword)
      oKey = BS.take keyLen hashed
  in rc4Encrypt oKey padString

computeFileKey :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> BS.ByteString
computeFileKey userPassword o p fileId keyLen =
  let padded = padPassword userPassword
      base = padded `BS.append` o `BS.append` int32LE p `BS.append` fileId
  in BS.take keyLen (md5 base)

computeUserU :: BS.ByteString -> BS.ByteString
computeUserU fileKey = rc4Encrypt fileKey padString

objectKey :: BS.ByteString -> Int -> Int -> Int -> BS.ByteString
objectKey fileKey objNum genNum keyLen =
  let ext = BS.take keyLen fileKey
            `BS.append` int24LE objNum
            `BS.append` int16LE genNum
  in BS.take (min (keyLen + 5) 16) (md5 ext)

hexString :: BS.ByteString -> BS.ByteString
hexString bs = BS.concat [BS.pack (printf "%02x" (fromEnum w)) | w <- BS.unpack bs]

pdfHex :: BS.ByteString -> BS.ByteString
pdfHex bs = BS.concat ["<", hexString bs, ">"]

encryptedRc4 :: BS.ByteString
encryptedRc4 =
  let text = "Hello encrypted"
      fileId = BS.pack $ map (chr . fromIntegral)
        [0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08
        ,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,0x10]
      keyLen = 5
      ownerPw = BS.pack "owner"
      userPw = BS.empty
      pPerm = -1
      oVal = computeOwnerO ownerPw keyLen
      fileKey = computeFileKey userPw oVal pPerm fileId keyLen
      uVal = computeUserU fileKey
      plainStream = BS.concat
        [ "BT /F1 24 Tf 72 720 Td (", text, ") Tj ET" ]
      encStream = rc4Encrypt (objectKey fileKey 4 0 keyLen) plainStream
      contentObj = BS.concat
        [ "<< /Length ", BS.pack (show (BS.length encStream)), " >>\nstream\n"
        , encStream
        , "endstream" ]
      encryptDict = BS.concat
        [ "<< /Filter /Standard /V 1 /R 2 /Length 40 /P ", BS.pack (show pPerm)
        , " /O ", pdfHex oVal
        , " /U ", pdfHex uVal
        , " >>" ]
      objects =
        [ (1, "<< /Type /Catalog /Pages 2 0 R >>")
        , (2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
        , (3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] \
              \/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
        , (4, contentObj)
        , (5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
        , (6, encryptDict)
        ]
      (body, offsets) = buildBody objects
      xrefPos = BS.length body
      trailer = BS.concat
        [ "trailer\n<< /Size 7 /Root 1 0 R /Encrypt 6 0 R /ID [ "
        , pdfHex fileId, " ", pdfHex fileId
        , " ] >>\nstartxref\n"
        , BS.pack (show xrefPos)
        , "\n%%EOF\n" ]
  in body <> xrefTable offsets 7 <> trailer

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
    , ("objstm.pdf", objstm)
    , ("indirect-length.pdf", indirectLength)
    , ("encrypted-rc4.pdf", encryptedRc4)
    , ("binary-endstream.pdf", binaryEndstream)
    ]
  where
    write dir (name, bytes) = do
      let path = dir </> name
      BS.writeFile path bytes
      putStrLn $ "wrote " ++ path ++ " (" ++ show (BS.length bytes) ++ " bytes)"
