{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module PDF.Encrypt
  ( Security
  , securityFromEncryptDict
  , decryptString
  , decryptStream
  , rc4KeyStream
  ) where

import PDF.Definition

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.List (foldl')
import qualified Data.Map as M
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Control.Monad (forM_, forM)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Word (Word8, Word32)
import Numeric (readHex)

import Crypto.Hash (hash, MD5(..), Digest)
import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher, cipherInit, ecbDecrypt)
import Crypto.Error (onCryptoFailure)
import Data.ByteArray (convert)

padString :: BS.ByteString
padString = BS.pack
  [ 0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41
  , 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08
  , 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80
  , 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A
  ]

data Security = Security
  { secRevision  :: Int
  , secVersion   :: Int
  , secKey       :: BS.ByteString
  , secKeyLength :: Int
  , secAES       :: Bool
  } deriving Show

securityFromEncryptDict :: Dict -> Dict -> Maybe String -> Maybe Security
securityFromEncryptDict encDict trailer password = do
  r <- dictInt encDict "/R"
  v <- dictInt encDict "/V"
  o <- dictBytes encDict "/O"
  u <- dictBytes encDict "/U"
  p <- dictInt encDict "/P"
  fileId <- dictFirstId trailer
  let pw = maybe BS.empty BSC.pack password
      aes = v >= 4 || usesAES encDict
      metaEnc = encryptMetadata encDict
      keyLen = case dictInt encDict "/Length" of
        Just n  -> max 5 (n `div` 8)
        Nothing -> if r >= 3 || v >= 2 then 16 else 5
  key <- authenticateFileKey pw o u p fileId r v aes metaEnc keyLen
  return $ Security r v key keyLen aes

-- | Try owner password first (Algorithm 7), then user password (Algorithm 6).
authenticateFileKey :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> Int -> Bool -> Bool -> Int -> Maybe BS.ByteString
authenticateFileKey pw o u p fileId r v aes metaEnc keyLen =
  ownerPasswordKey pw o u p fileId r aes metaEnc keyLen
  <|> userPasswordKey pw o u p fileId r aes metaEnc keyLen

userPasswordKey :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> Bool -> Bool -> Int -> Maybe BS.ByteString
userPasswordKey pw o u p fileId r aes metaEnc keyLen =
  let key = fileKeyFromPassword pw o p fileId r aes metaEnc keyLen
  in if verifyUserPassword r fileId key u then Just key else Nothing

ownerPasswordKey :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> Bool -> Bool -> Int -> Maybe BS.ByteString
ownerPasswordKey ownerPw o u p fileId r aes metaEnc keyLen =
  let oKey = computeOwnerValueKey ownerPw r keyLen
      userPw = decryptOToUserPassword oKey r o
  in userPasswordKey userPw o u p fileId r aes metaEnc keyLen

fileKeyFromPassword :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> Bool -> Bool -> Int -> BS.ByteString
fileKeyFromPassword pw o p fileId r aes metaEnc keyLen =
  if r >= 4 || aes
  then computeFileKeyAES pw o p fileId r metaEnc
  else computeFileKey pw o p fileId r keyLen metaEnc

padPassword :: BS.ByteString -> BS.ByteString
padPassword pw = BS.take 32 $ pw `BS.append` padString

-- Algorithm 3 (a–d): RC4 key from owner password for decrypting /O.
computeOwnerValueKey :: BS.ByteString -> Int -> Int -> BS.ByteString
computeOwnerValueKey ownerPassword r keyLen =
  let hashed = iterate md5 (md5 (padPassword ownerPassword)) !! (if r >= 3 then 50 else 0)
  in BS.take keyLen hashed

-- Algorithm 7 (b): recover padded user password from /O.
decryptOToUserPassword :: BS.ByteString -> Int -> BS.ByteString -> BS.ByteString
decryptOToUserPassword oKey r o
  | r <= 2    = rc4Decrypt oKey o
  | otherwise = foldl' (\ct m -> rc4Decrypt (xorKey oKey m) ct) o [19,18..0]

usesAES :: Dict -> Bool
usesAES d = case dictLookup d "/CF" of
  Just (PdfDict cf) -> case dictLookup cf "/StdCF" of
    Just (PdfDict std) -> case dictLookup std "/CFM" of
      Just (PdfName "/AESV2") -> True
      Just (PdfName "/AESV3") -> True
      _ -> False
    _ -> False
  _ -> False

encryptMetadata :: Dict -> Bool
encryptMetadata d = case dictLookup d "/EncryptMetadata" of
  Just (PdfBool False) -> False
  _ -> True

dictLookup :: Dict -> T.Text -> Maybe Obj
dictLookup d name = M.lookup name d

dictInt :: Dict -> T.Text -> Maybe Int
dictInt d name = case dictLookup d name of
  Just (PdfNumber n) -> Just (truncate n)
  _ -> Nothing

dictBytes :: Dict -> T.Text -> Maybe BS.ByteString
dictBytes d name = case dictLookup d name of
  Just (PdfText s) -> Just (BSC.pack (T.unpack s))
  Just (PdfHex h)   -> hexToBytes h
  _ -> Nothing

dictFirstId :: Dict -> Maybe BS.ByteString
dictFirstId d = case dictLookup d "/ID" of
  Just (PdfArray (entry:_)) -> idEntryBytes entry
  _ -> Nothing

idEntryBytes :: Obj -> Maybe BS.ByteString
idEntryBytes (PdfHex h)   = hexToBytes h
idEntryBytes (PdfText s)  = hexToBytes s <|> Just (BSC.pack (T.unpack s))
idEntryBytes _            = Nothing

hexToBytes :: T.Text -> Maybe BS.ByteString
hexToBytes h
  | T.null h = Just BS.empty
  | otherwise =
      let chunks = pairs (T.unpack h)
          bytes = mapMaybe parseHexPair chunks
      in if length bytes == length chunks then Just (BS.pack bytes) else Nothing
  where
    pairs [] = []
    pairs s  = take 2 s : pairs (drop 2 s)
    parseHexPair [a, b] =
      case readHex [a, b] of
        [(n, "")] -> Just (fromIntegral n)
        _         -> Nothing
    parseHexPair [a] =
      case readHex [a, '0'] of
        [(n, "")] -> Just (fromIntegral n)
        _         -> Nothing
    parseHexPair _ = Nothing

md5 :: BS.ByteString -> BS.ByteString
md5 bs = convert (hash bs :: Digest MD5)

computeFileKey :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> Int -> Bool -> BS.ByteString
computeFileKey password o p fileId r keyLen metaEnc =
  let padded = BS.take 32 $ password `BS.append` padString
      suffix = if r >= 4 && not metaEnc then BS.pack [0xFF, 0xFF, 0xFF, 0xFF] else BS.empty
      base = padded `BS.append` o `BS.append` int32LE p `BS.append` fileId `BS.append` suffix
      hashed = iterate (\h -> md5 (BS.take keyLen h)) (md5 base) !! (if r >= 3 then 50 else 0)
  in BS.take keyLen hashed

computeFileKeyAES :: BS.ByteString -> BS.ByteString -> Int -> BS.ByteString -> Int -> Bool -> BS.ByteString
computeFileKeyAES password o p fileId r metaEnc =
  let padded = BS.take 32 $ password `BS.append` padString
      suffix = if r >= 4 && not metaEnc then BS.pack [0xFF, 0xFF, 0xFF, 0xFF] else BS.empty
      base = padded `BS.append` o `BS.append` int32LE p `BS.append` fileId `BS.append` suffix
      hashed = (!! 50) $ iterate (\h -> md5 (BS.take 16 h)) (md5 base)
  in BS.take 16 hashed

int32LE :: Int -> BS.ByteString
int32LE n =
  let w = fromIntegral n :: Word32
  in BS.pack
  [ fromIntegral (w .&. 0xff)
  , fromIntegral ((w `shiftR` 8) .&. 0xff)
  , fromIntegral ((w `shiftR` 16) .&. 0xff)
  , fromIntegral ((w `shiftR` 24) .&. 0xff)
  ]

verifyUserPassword :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
verifyUserPassword r fileId key u =
  let computed = computeU r fileId key
  in if r >= 3
     then BS.take 16 computed == BS.take 16 u
     else BS.take 32 computed == BS.take 32 u

computeU :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
computeU r fileId key
  | r <= 2 = rc4Decrypt key padString
  | otherwise =
      let hashed = md5 (padString `BS.append` fileId)
          encrypted = rc4Decrypt key hashed
          finished = foldl' (\ct i -> rc4Decrypt (xorKey key i) ct) encrypted [1..19]
      in BS.append (BS.take 16 finished) (BS.replicate 16 0)

xorKey :: BS.ByteString -> Int -> BS.ByteString
xorKey key i = BS.pack $ map (xor (fromIntegral i)) (BS.unpack key)

saltAES :: BS.ByteString
saltAES = BSC.pack "sAlT"

objectKey :: Security -> Int -> Int -> BS.ByteString
objectKey sec objNum genNum =
  let n = secKeyLength sec
      ext = BS.take n (secKey sec)
            `BS.append` int24LE objNum
            `BS.append` int16LE genNum
  in if secAES sec
     then BS.take (min (n + 5) 16) (md5 (ext `BS.append` saltAES))
     else BS.take (min (n + 5) 16) (md5 ext)

int24LE :: Int -> BS.ByteString
int24LE n =
  let w = fromIntegral n :: Word32
  in BS.pack
  [ fromIntegral (w .&. 0xff)
  , fromIntegral ((w `shiftR` 8) .&. 0xff)
  , fromIntegral ((w `shiftR` 16) .&. 0xff)
  ]

int16LE :: Int -> BS.ByteString
int16LE n =
  let w = fromIntegral n :: Word32
  in BS.pack
  [ fromIntegral (w .&. 0xff)
  , fromIntegral ((w `shiftR` 8) .&. 0xff)
  ]

rc4Decrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString
rc4Decrypt key ciphertext =
  let ks = rc4KeyStream key (BS.length ciphertext)
  in BS.pack $ zipWith xor (BS.unpack ciphertext) (BS.unpack ks)

rc4DecryptRev3 :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
rc4DecryptRev3 keyLen key ciphertext =
  foldl' (\ct m -> rc4Decrypt (BS.take keyLen (md5 (key `BS.append` BS.pack [fromIntegral m]))) ct)
         ciphertext
         [19,18..0]

rc4KeyStream :: BS.ByteString -> Int -> BS.ByteString
rc4KeyStream _ nbytes | nbytes <= 0 = BS.empty
rc4KeyStream key nbytes = runST $ do
  let keyLen = BS.length key
  sbox <- newArray (0, 255) 0 :: ST s (STUArray s Int Word8)
  forM_ [0..255] $ \i -> writeArray sbox i (fromIntegral i :: Word8)
  jRef <- newSTRef (0 :: Int)
  forM_ [0..255] $ \i -> do
    j <- readSTRef jRef
    si <- fmap fromIntegral (readArray sbox i)
    kj <- return $ fromIntegral (BS.index key (i `mod` keyLen))
    let j' = (j + si + kj) `mod` 256
    rc4Swap sbox i j'
    writeSTRef jRef j'
  out <- (newArray (0, nbytes - 1) (0 :: Word8) :: ST s (STUArray s Int Word8))
  iRef <- newSTRef 0
  jRef2 <- newSTRef 0
  forM_ [0..nbytes - 1] $ \n -> do
    i <- readSTRef iRef
    j <- readSTRef jRef2
    let i' = (i + 1) `mod` 256
    si <- fmap fromIntegral (readArray sbox i')
    let j' = (j + si) `mod` 256
    rc4Swap sbox i' j'
    a <- fmap fromIntegral (readArray sbox i')
    b <- fmap fromIntegral (readArray sbox j')
    byte <- readArray sbox ((a + b) `mod` 256)
    writeArray out n byte
    writeSTRef iRef i'
    writeSTRef jRef2 j'
  arr <- forM [0..nbytes - 1] (readArray out)
  return (BS.pack arr)

rc4Swap :: STUArray s Int Word8 -> Int -> Int -> ST s ()
rc4Swap arr i j = do
  a <- readArray arr i
  b <- readArray arr j
  writeArray arr i b
  writeArray arr j a

decryptString :: Maybe Security -> Int -> Int -> BS.ByteString -> BS.ByteString
decryptString Nothing _ _ bs = bs
decryptString (Just sec) objNum genNum bs =
  let ok = objectKey sec objNum genNum
  in case () of
    _ | secAES sec -> stripPkcs7 $ aesDecrypt ok bs
    _ -> rc4Decrypt ok bs

decryptStream :: Maybe Security -> Int -> Int -> BS.ByteString -> BS.ByteString
decryptStream Nothing _ _ bs = bs
decryptStream (Just sec) objNum genNum bs =
  let ok = objectKey sec objNum genNum
  in case () of
    _ | secAES sec -> stripPkcs7 $ aesDecrypt ok bs
    _ -> rc4Decrypt ok bs

stripPkcs7 :: BS.ByteString -> BS.ByteString
stripPkcs7 bs
  | BS.null bs = bs
  | otherwise =
      let pad = fromIntegral (BS.last bs)
          len = BS.length bs
      in if pad >= 1 && pad <= 16 && len >= pad
            && BS.all (== BS.last bs) (BS.drop (len - pad) bs)
         then BS.take (len - pad) bs
         else bs

aesDecrypt :: BS.ByteString -> BS.ByteString -> BS.ByteString
aesDecrypt _ bs | BS.length bs < 16 = bs
aesDecrypt key bs =
  onCryptoFailure (const bs) id $
    cipherInit key >>= \cipher ->
      let iv = BS.take 16 bs
          body = BS.drop 16 bs
      in return $ cbcDecrypt iv cipher body

cbcDecrypt :: BS.ByteString -> AES128 -> BS.ByteString -> BS.ByteString
cbcDecrypt _ _ bs | BS.null bs = BS.empty
cbcDecrypt prev cipher bs =
  let (block, rest) = BS.splitAt 16 bs
      plain = ecbDecrypt cipher block
      out = BS.pack $ zipWith xor (BS.unpack plain) (BS.unpack prev)
  in if BS.length block < 16
     then BS.empty
     else out `BS.append` cbcDecrypt block cipher rest
