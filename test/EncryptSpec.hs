{-# LANGUAGE OverloadedStrings #-}

module EncryptSpec (encryptSpecCases) where

import PDF.Encrypt (rc4KeyStream)

import Data.Word (Word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

encryptSpecCases :: [(String, BS.ByteString, BS.ByteString)]
encryptSpecCases =
  let keyStream = rc4KeyStream (BSC.pack "Key") 10
   in [ ( "rc4KeyStream Key deterministic"
        , keyStream
        , keyStream
        )
      , ( "rc4KeyStream empty for zero length"
        , BS.empty
        , rc4KeyStream (BSC.pack "Key") 0
        )
      , ( "rc4KeyStream five-byte key prefix"
        , BS.pack ([0xB2, 0x39, 0x63, 0x05, 0xF0] :: [Word8])
        , rc4KeyStream (BS.pack [1, 2, 3, 4, 5]) 5
        )
      ]
