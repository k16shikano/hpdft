module PDF.Definition where

import Data.ByteString (ByteString)
import Data.List (replicate, intercalate)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Codec.Compression.Zlib (decompress)
import Data.Map (Map)
import qualified Data.Map as M
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
         | PdfText String 
         | PdfStream PDFStream
         | PdfNumber Double 
         | PdfHex String
         | PdfBool Bool
         | PdfArray [Obj]
         | PdfName String 
         | ObjRef Int
         | ObjOther String
         | PdfNull
         deriving (Eq, Show)

type Dict = Map String Obj

ppObj :: Obj -> String
ppObj = ppObjAt 0

ppDict :: Int -> Dict -> String
ppDict depth d = concat $ map dictentry (M.toList d)
  where dictentry (n, o) =
          concat $ ["\n"] ++ replicate depth "  " ++ [n, ": ", ppObjAt (depth+1) o]

ppDictEntries :: Dict -> String
ppDictEntries d =
  "[" ++ intercalate "," (map (\(k, v) -> "(" ++ k ++ "," ++ ppObj v ++ ")") (M.toList d)) ++ "]"

ppObjAt :: Int -> Obj -> String
ppObjAt depth (PdfDict d) = ppDict depth d
ppObjAt _ (PdfText t) = t
ppObjAt _ (PdfStream s) = "\n  " ++ BSL.unpack s
ppObjAt _ (PdfNumber r) = show r
ppObjAt _ (PdfHex h) = h
ppObjAt depth (PdfArray a) = intercalate ", " $ map (ppObjAt depth) a
ppObjAt _ (PdfBool b) = show b
ppObjAt _ (PdfName n) = n
ppObjAt _ (ObjRef i) = show i
ppObjAt _ (ObjOther o) = o
ppObjAt _ PdfNull = ""


data Encoding = CIDmap String | Encoding (Map Char String) | WithCharSet String | NullMap

instance Show Encoding where
  show (CIDmap s) = "CIDmap"++s
  show (Encoding a) = "Encoding"++show a
  show (WithCharSet s) = "WithCharSet"++s
  show NullMap = []

type CMap = Map Int String

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
               , curfont    :: String
               , cmaps      :: Map String CMap
               , fontmaps   :: Map String Encoding
               , colorspace :: String
               , xcolorspaces :: [String]
               , warnings     :: [PdfWarning]
               }
         deriving (Show)

