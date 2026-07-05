{-# LANGUAGE OverloadedStrings #-}

module PDF.Definition where

import Data.ByteString (ByteString)
import Data.List (replicate, intercalate)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Codec.Compression.Zlib (decompress)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import PDF.Error (PdfWarning(..))

type PDFBS = (Int,BS.ByteString)

data XrefEntry = InFile Int
               | InObjStm Int Int
               deriving (Eq, Show)

type XREF = Map Int XrefEntry

type PDFObj = (Int,[Obj])

type PDFObjIndex = Map Int [Obj]

type PDFStream = BSL.ByteString

data Obj = PdfDict Dict
         | PdfText T.Text
         | PdfStream PDFStream
         | PdfNumber Double
         | PdfHex T.Text
         | PdfBool Bool
         | PdfArray [Obj]
         | PdfName T.Text
         | ObjRef Int
         | ObjOther T.Text
         | PdfNull
         deriving (Eq, Show)

type Dict = Map T.Text Obj

ppObj :: Obj -> String
ppObj = ppObjAt 0

ppDict :: Int -> Dict -> String
ppDict depth d = concat $ map dictentry (M.toList d)
  where dictentry (n, o) =
          concat $ ["\n"] ++ replicate depth "  " ++ [T.unpack n, ": ", ppObjAt (depth+1) o]

ppDictEntries :: Dict -> String
ppDictEntries d =
  "[" ++ intercalate "," (map (\(k, v) -> "(" ++ T.unpack k ++ "," ++ ppObj v ++ ")") (M.toList d)) ++ "]"

ppObjAt :: Int -> Obj -> String
ppObjAt depth (PdfDict d) = ppDict depth d
ppObjAt _ (PdfText t) = T.unpack t
ppObjAt _ (PdfStream s) = "\n  " ++ BSL.unpack s
ppObjAt _ (PdfNumber r) = show r
ppObjAt _ (PdfHex h) = T.unpack h
ppObjAt depth (PdfArray a) = intercalate ", " $ map (ppObjAt depth) a
ppObjAt _ (PdfBool b) = show b
ppObjAt _ (PdfName n) = T.unpack n
ppObjAt _ (ObjRef i) = show i
ppObjAt _ (ObjOther o) = T.unpack o
ppObjAt _ PdfNull = ""


data Encoding = CIDmap T.Text | Encoding (Map Char T.Text) | WithCharSet T.Text | NullMap

instance Show Encoding where
  show (CIDmap s) = "CIDmap" ++ T.unpack s
  show (Encoding a) = "Encoding" ++ show a
  show (WithCharSet s) = "WithCharSet" ++ T.unpack s
  show NullMap = []

type CMap = Map Int T.Text

data FontInfo = FontInfo
  { fiEncoding     :: Encoding
  , fiToUnicode    :: CMap
  , fiWidth        :: Int -> Double
  , fiWidthV       :: Int -> Double
  , fiWMode        :: Int
  , fiBytesPerCode :: Int
  , fiDefaultWidth :: Double
  }

data PSR = PSR { linex      :: Double
               , liney      :: Double
               , absolutex  :: Double
               , absolutey  :: Double
               , text_lm    :: (Double, Double, Double, Double, Double, Double)
               , text_m     :: (Double, Double, Double, Double, Double, Double)
               , text_break :: Bool
               , leftmargin :: Double
               , top        :: Double
               , bottom     :: Double
               , fontfactor :: Double
               , curfont    :: T.Text
               , cmaps      :: Map T.Text CMap
               , fontmaps   :: Map T.Text Encoding
               , colorspace :: T.Text
               , xcolorspaces :: [T.Text]
               , warnings     :: [PdfWarning]
               }
         deriving (Show)
