module PDF.Definition where

import Data.ByteString (ByteString)
import Data.List (replicate, intercalate)
import Data.Word
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
--toString depth (PdfStream s) = "\n  " ++ (BSL.unpack $ decompress s)
toString depth (PdfStream s) = "\n  " ++ (BSL.unpack $ s)
toString depth (PdfNumber r) = show r
toString depth (PdfHex h) = h 
toString depth (PdfArray a) = intercalate ", " $ map (toString depth) a
toString depth (PdfBool b) = show b
toString depth (PdfName n) = n
toString depth (ObjRef i) = show i
toString depth (ObjOther o) = o
toString depth (PdfNull) = ""


data FontMap = CIDmap String | FontMap [(Char,String)] | WithCharSet String | NullMap

instance Show FontMap where
  show (CIDmap s) = "CIDmap"++s
  show (FontMap a) = "FontMap"++show a
  show (WithCharSet s) = "WithCharSet"++s
  show NullMap = []

type CMap = [(Int,String)]

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
               , cmaps      :: [(String, CMap)]
               , fontmaps   :: [(String, FontMap)]
               , colorspace :: String
               , xcolorspaces :: [String]
               , decryption :: ENC
               }
         deriving (Show)

data ENC = ENC { pass :: [Word8]
               , filter :: Obj -- PdfName
               , subfilter :: Obj -- PdfName 
               , v :: Int
               , length :: Int
               , cf :: Obj -- PdfDict
               , stfm :: Obj -- PdfName
               , strm :: Obj -- PdfName
               , eff :: Obj
               , r :: Int
               , o :: [Word8]
               , u :: [Word8]
               , p :: Int
               , meta :: Bool
               }
           | NoENCRYPT
         deriving (Show)
