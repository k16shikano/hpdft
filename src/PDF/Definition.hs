module PDF.Definition where

import Data.ByteString (ByteString)
import Data.List (replicate, intercalate)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Codec.Compression.Zlib (decompress)

type PDFBS = (Int,BS.ByteString)

type PDFObj = (Int,[Obj])

type PDFStream = BSL.ByteString

type PDFxref = BSL.ByteString

data Obj = PdfDict Dict -- [(Obj, Obj)]
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
         deriving (Eq)

type Dict =  [(Obj,Obj)]

instance Show Obj where
  show o = toString 0 o
  
toString depth (PdfDict d) = concat $ map dictentry d
    where dictentry (PdfName n, o) = concat $ ["\n"] ++ replicate depth "  " ++ [n, ": ", toString (depth+1) o]
          dictentry e = error $ "Illegular dictionary entry "++show e 
toString depth (PdfText t) = t 
toString depth (PdfStream s) = "\n  " ++ (BSL.unpack $ decompress s)
--toString depth (PdfStream s) = "\n  " ++ (BSL.unpack $ s)
toString depth (PdfNumber r) = show r
toString depth (PdfHex h) = h 
toString depth (PdfArray a) = intercalate ", " $ map (toString depth) a
toString depth (PdfBool b) = show b
toString depth (PdfName n) = n
toString depth (ObjRef i) = show i
toString depth (ObjOther o) = o
toString depth (PdfNull) = ""


type FontMap = [(Char,String)]

type CMap = [(Int,String)]

data PSR = PSR { linex      :: Double
               , liney      :: Double
               , absolutex  :: Double
               , absolutey  :: Double
               , leftmargin :: Double
               , top        :: Double
               , bottom     :: Double
               , fontfactor :: Double
               , curfont    :: String
               , cmaps      :: [(String, CMap)]
               , fontmaps   :: [(String, FontMap)]
               , colorspace :: String
               , xcolorspaces :: [String]
               }
         deriving (Show)

