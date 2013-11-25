module Pdf where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

type PDFBS = (Int,BS.ByteString)

type PDFObj = (Int,[Obj])

type PDFStream = BSL.ByteString

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
         deriving (Show, Eq)

type Dict =  [(Obj,Obj)]

type FontMap = [(Char,String)]

data PSR = PSR { linex      :: Double
               , liney      :: Double
               , absolutex  :: Double
               , absolutey  :: Double
               , leftmargin :: Double
               , top        :: Double
               , bottom     :: Double
               , curfont    :: String
               , fontmaps   :: [(String, FontMap)]}
         deriving (Show)


