{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : PDF.Error
Description : Error and warning types for hpdft
License     : MIT
Maintainer  : k16.shikano@gmail.com

Total-function error model for the 0.3 API. Parsing and extraction
functions return 'PdfResult' instead of calling 'error', so that a
broken PDF yields a diagnosis (and possibly partial output) rather
than a crash.

Migration plan (tracked per module):

* Phase A1: xref\/trailer\/object loading ('PDF.DocumentStructure', 'PDF.Object')
* Phase A2: content streams and fonts ('PDF.ContentStream', 'PDF.Cmap', 'PDF.CFF', ...)
* Phase A3: IO layer and CLI ('PDF.PDFIO', @hpdft.hs@)
-}

module PDF.Error
  ( PdfError(..)
  , PdfResult
  , PdfWarning(..)
  , renderPdfWarning
  , note
  ) where

import qualified Data.ByteString.Char8 as BS

-- | Structured errors. Constructors carry enough context to locate
-- the problem in the file without a debugger.
data PdfError
  = ParseError String BS.ByteString
    -- ^ Parser failure: message and a short excerpt of the input.
  | BrokenXref String
    -- ^ Cross-reference table\/stream could not be interpreted.
  | MissingObject Int
    -- ^ An object referenced as @n 0 R@ is not present.
  | MissingKey String String
    -- ^ Required dictionary key is absent: key name and context.
  | UnsupportedFeature String
    -- ^ Valid PDF that hpdft cannot handle yet (filter, encoding, ...).
  | DecryptionError String
    -- ^ Bad password, unsupported handler, or malformed \/Encrypt.
  | FontError Int String
    -- ^ Font-related failure: object number and description.
  deriving (Show, Eq)

type PdfResult a = Either PdfError a

-- | Non-fatal diagnoses. Extraction continues; the caller may log these.
data PdfWarning
  = UnknownOperator String
    -- ^ Content-stream operator hpdft does not interpret.
  | MissingToUnicode Int
    -- ^ Font without usable \/ToUnicode; output may be garbled.
  | SubstitutedEncoding Int String
    -- ^ Fallback encoding used for a font.
  | UnmappedCid Int
    -- ^ CID not found in Adobe-Japan1-6; bracket placeholder emitted.
  deriving (Show, Eq)

renderPdfWarning :: PdfWarning -> String
renderPdfWarning (UnknownOperator op) = "unknown content-stream operator: " ++ op
renderPdfWarning (MissingToUnicode n) =
  "font object " ++ show n ++ " has no usable /ToUnicode map"
renderPdfWarning (SubstitutedEncoding n enc) =
  "font object " ++ show n ++ ": using fallback encoding " ++ enc
renderPdfWarning (UnmappedCid cid) =
  "unmapped CID " ++ show cid ++ " (Adobe-Japan1-6)"

-- | Annotate a 'Maybe' with an error.
note :: PdfError -> Maybe a -> PdfResult a
note e Nothing  = Left e
note _ (Just a) = Right a
