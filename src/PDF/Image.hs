{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Image
Description : Image XObject extraction from PDF pages (0.4.3)
License     : MIT

Extracts '/Image' XObjects referenced on a page (including nested Form
XObjects). Inline images are not supported in 0.4.3.

@example
import PDF.Image (extractPageImages)

images <- extractPageImages doc 1
-- each PageImage carries format, bytes, and placement bbox
-}
module PDF.Image
  ( ImageFormat(..)
  , PageImage(..)
  , extractPageImages
  , extractPageImagesToDir
  , classifyImageBytes
  , encodePngRgb
  , encodePngGray
  ) where

import PDF.Definition (Dict, Obj(..), PDFObjIndex)
import PDF.Document (Document(..))
import PDF.DocumentStructure
  ( findDict
  , findDictByRef
  , findObjsByRef
  , rawStreamByRef
  , streamFilterNames
  )
import PDF.Error (PdfError(..), PdfResult)
import PDF.Interpret (Rect(..), interpretPageImageHits)
import PDF.Page (pageRefAt)

import Codec.Compression.Zlib (compressWith, defaultCompressParams)

import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.Char (ord)
import Data.Word (Word32, Word8)

import Control.Exception (SomeException, catch)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T

data ImageFormat = ImageJPEG | ImagePNG | ImageRaw
  deriving (Eq, Show)

data PageImage = PageImage
  { piIndex  :: Int
  , piPage   :: Int
  , piBBox   :: Rect
  , piFormat :: ImageFormat
  , piBytes  :: BS.ByteString
  } deriving (Eq, Show)

-- | Extract image XObjects from a 1-based page number.
extractPageImages :: Document -> Int -> PdfResult [PageImage]
extractPageImages doc pageNum = do
  pref <- pageRefAt doc pageNum
  hits <- interpretPageImageHits doc pref
  mapM (loadHit doc pageNum) (zip [1 ..] hits)

extractPageImagesToDir :: Document -> Int -> FilePath -> IO (PdfResult [FilePath])
extractPageImagesToDir doc pageNum outDir =
  createDirectoryIfMissing True outDir >> go
  where
    go = case extractPageImages doc pageNum of
      Left err -> return (Left err)
      Right images -> fmap Right (mapM (writePageImage outDir) images)

loadHit :: Document -> Int -> (Int, (Int, Rect)) -> PdfResult PageImage
loadHit doc pageNum (idx, (ref, bbox)) = do
  (fmt, bytes) <- loadImageBytes doc ref
  return PageImage
    { piIndex = idx
    , piPage = pageNum
    , piBBox = bbox
    , piFormat = fmt
    , piBytes = bytes
    }

loadImageBytes :: Document -> Int -> PdfResult (ImageFormat, BS.ByteString)
loadImageBytes doc ref = do
  os <- case findObjsByRef ref (docObjs doc) of
    Just x  -> Right x
    Nothing -> Left (MissingObject ref)
  d <- case findDict os of
    Just x  -> Right x
    Nothing -> Left (MissingKey "/Type" ("image object " ++ show ref))
  stream <- case M.lookup ref (docStreamCache doc) of
    Just r -> r
    Nothing -> rawStreamByRef (docSecurity doc) (docObjs doc) ref
  classifyImageBytes (docObjs doc) d (BSL.toStrict stream)

classifyImageBytes
  :: PDFObjIndex -> Dict -> BS.ByteString -> PdfResult (ImageFormat, BS.ByteString)
classifyImageBytes objs d bs = do
  filters <- streamFilterNames d
  if "/DCTDecode" `elem` map T.unpack filters || isJpeg bs
    then return (ImageJPEG, bs)
    else do
      w <- dictPositiveInt d "/Width"
      h <- dictPositiveInt d "/Height"
      bpc <- dictPositiveInt d "/BitsPerComponent"
      comps <- colorSpaceComponents objs d
      case (comps, bpc, BS.length bs) of
        (Just 3, 8, n) | n == w * h * 3 ->
          encodePngRgb w h bs >>= \png -> return (ImagePNG, png)
        (Just 1, 8, n) | n == w * h ->
          encodePngGray w h bs >>= \png -> return (ImagePNG, png)
        _ -> return (ImageRaw, bs)

isJpeg :: BS.ByteString -> Bool
isJpeg bs =
  BS.length bs >= 3
    && BS.index bs 0 == 0xff
    && BS.index bs 1 == 0xd8
    && BS.index bs 2 == 0xff

dictPositiveInt :: Dict -> T.Text -> PdfResult Int
dictPositiveInt d key = case M.lookup key d of
  Just (PdfNumber n) ->
    let i = truncate n
     in if i > 0 then Right i else Left (UnsupportedFeature ("invalid " ++ T.unpack key))
  _ -> Left (MissingKey (T.unpack key) "image XObject")

colorSpaceComponents :: PDFObjIndex -> Dict -> PdfResult (Maybe Int)
colorSpaceComponents objs d = case M.lookup "/ColorSpace" d of
  Nothing -> Right Nothing
  Just (PdfName "/DeviceRGB") -> Right (Just 3)
  Just (PdfName "/DeviceGray") -> Right (Just 1)
  Just (ObjRef r) ->
    case findDictByRef r objs of
      Just csDict -> colorSpaceComponents objs csDict
      Nothing -> Right Nothing
  Just (PdfArray (PdfName "/DeviceRGB" : _)) -> Right (Just 3)
  Just (PdfArray (PdfName "/DeviceGray" : _)) -> Right (Just 1)
  Just (PdfArray (ObjRef r : _)) ->
    case findDictByRef r objs of
      Just csDict -> colorSpaceComponents objs csDict
      Nothing -> Right Nothing
  _ -> Right Nothing

writePageImage :: FilePath -> PageImage -> IO FilePath
writePageImage outDir img = do
  let base = outDir </> pageImageBasename img
      (path, sidecar) = case piFormat img of
        ImageJPEG -> (base ++ ".jpg", Nothing)
        ImagePNG  -> (base ++ ".png", Nothing)
        ImageRaw  -> (base ++ ".raw", Just (base ++ ".json"))
  writeBinary path (piBytes img)
  case sidecar of
    Nothing -> return ()
    Just sc -> writeBinary sc (rawSidecar img)
  return path

pageImageBasename :: PageImage -> String
pageImageBasename PageImage{piPage = pg, piIndex = idx} =
  "page" ++ show pg ++ "-" ++ pad3 idx

pad3 :: Int -> String
pad3 n =
  let s = show n
   in replicate (max 0 (3 - length s)) '0' ++ s

writeBinary :: FilePath -> BS.ByteString -> IO ()
writeBinary path bs =
  BS.writeFile path bs `catch` handleWriteErr
  where
    handleWriteErr :: SomeException -> IO ()
    handleWriteErr e =
      ioError (userError ("cannot write file: " ++ path ++ ": " ++ show e))

rawSidecar :: PageImage -> BS.ByteString
rawSidecar PageImage{piPage = pg, piIndex = idx, piBBox = bbox, piBytes = bs} =
  BSC.pack json
  where
    json :: String
    json =
      "{\"page\":" ++ show pg
        ++ ",\"index\":" ++ show idx
        ++ ",\"format\":\"raw\""
        ++ ",\"length\":" ++ show (BS.length bs)
        ++ ",\"bbox\":[" ++ show (rectX0 bbox) ++ "," ++ show (rectY0 bbox)
        ++ "," ++ show (rectX1 bbox) ++ "," ++ show (rectY1 bbox) ++ "]}"

-- | Minimal PNG encoder (filter 0 scanlines, zlib IDAT). No new dependencies.
encodePngRgb :: Int -> Int -> BS.ByteString -> PdfResult BS.ByteString
encodePngRgb w h pixels = encodePng 8 2 w h (pngScanlines w 3 pixels)

encodePngGray :: Int -> Int -> BS.ByteString -> PdfResult BS.ByteString
encodePngGray w h pixels = encodePng 8 0 w h (pngScanlines w 1 pixels)

encodePng :: Word8 -> Word8 -> Int -> Int -> BS.ByteString -> PdfResult BS.ByteString
encodePng bitDepth colorType w h scanData = do
  let ihdr =
        word32BE (fromIntegral w)
          <> word32BE (fromIntegral h)
          <> BS.pack [bitDepth, colorType, 0, 0, 0]
      idat = BSL.toStrict (compressWith defaultCompressParams (BSL.fromStrict scanData))
  return $
    pngSignature
      <> pngChunk "IHDR" ihdr
      <> pngChunk "IDAT" idat
      <> pngChunk "IEND" BS.empty

pngSignature :: BS.ByteString
pngSignature = BS.pack [137, 80, 78, 71, 13, 10, 26, 10]

pngScanlines :: Int -> Int -> BS.ByteString -> BS.ByteString
pngScanlines w comps pixels =
  BS.concat
    [ BS.cons 0 (BS.take (w * comps) (BS.drop (row * w * comps) pixels))
    | row <- [0 .. rows - 1]
    ]
  where
    rows = BS.length pixels `div` (w * comps)

pngChunk :: String -> BS.ByteString -> BS.ByteString
pngChunk tag payload =
  let tagBs = BS.pack (map (fromIntegral . ord) tag)
      len = word32BE (fromIntegral (BS.length payload))
      body = tagBs <> payload
      crc = word32BE (crc32 body)
   in len <> body <> crc

word32BE :: Word32 -> BS.ByteString
word32BE w =
  BS.pack
    [ fromIntegral ((w `shiftR` 24) .&. 0xff)
    , fromIntegral ((w `shiftR` 16) .&. 0xff)
    , fromIntegral ((w `shiftR` 8) .&. 0xff)
    , fromIntegral (w .&. 0xff)
    ]

crc32 :: BS.ByteString -> Word32
crc32 bs = finish $ BS.foldl' step 0xffffffff bs
  where
    step crc b =
      (crc `shiftR` 8)
        `xor` (crcTable !! fromIntegral ((crc `xor` fromIntegral b) .&. 0xff))
    finish crc = crc `xor` 0xffffffff

crcTable :: [Word32]
crcTable =
  [ foldl
      (\c _ ->
         if c .&. 1 /= 0
           then xor 0xedb88320 (c `shiftR` 1)
           else c `shiftR` 1
      )
      (fromIntegral n)
      [0 .. 7 :: Int]
  | n <- [0 .. 255]
  ]
