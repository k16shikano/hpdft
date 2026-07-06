{-# LANGUAGE OverloadedStrings #-}
import PDF.Matrix (Matrix, identity, multiply, apply, applyVec, translate, scale, mkMatrix, components)
import PDF.Definition (Obj(..), FontInfo(..), Encoding(..))
import PDF.StreamLex
  ( normalizePdfNumber
  , parsePdfNumber
  , unicodeBytesToCodes
  )
import PDF.StreamLex
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions, needsAozoraBar, aozoraRuby, layoutParagraphs, layoutParagraphsWith, layoutPageText, layoutDocument, sortLinesByReadingOrder, linesFromGlyphs, Line(..))
import PDF.Structure (StructElem(..), StructKid(..), structTree, logicalOrder)
import PDF.Document (Document(..), openDocument)
import PDF.Page (pageCount, pageRefAt, pageParagraphs)
import PDF.Diff (TextChange(..), compareDocuments, diffParagraphs)
import PDF.DocumentStructure (parseCIDWidths, simpleWidthAt, decodeStreamBytes)
import PDF.Character (jisx0208Map)
import PDF.Image
  ( ImageFormat(..)
  , PageImage(..)
  , classifyImageBytes
  , encodePngRgb
  , extractPageImages
  )
import PDF.FormExtract (pageFormNames, extractFormPdf)
import PDF.Text (pdfToTextTaggedBS, pdfToTextDoc, pdfToTextStreamDoc)
import PDF.Error (PdfResult)

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T

import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath ((</>))
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, modifyIORef)
import PDF.Interpret
  ( Glyph(..)
  , Rect(..)
  , PageItem(..)
  , interpretContentWithFonts
  , interpretContentWithFontsItems
  , interpretPageImageHits
  , bytesToCodes
  , encodingUnicode
  )
import EncryptSpec (encryptSpecCases)
import TuiGeometry (HeightSpec(..), parseHeightSpec, viewportHeight)
import TuiScroll
  ( ScrollState(..)
  , initialScrollState
  , scrollBy
  , scrollToTop
  , scrollToEnd
  , scrollHalfPageDown
  , scrollHalfPageUp
  , searchForwardFrom
  , searchBackwardFrom
  , visibleLineRange
  , statusLineNumber
  , stringDisplayWidth
  , clipToDisplayWidth
  , padToDisplayWidth
  )

import Data.Word (Word8)

epsilon :: Double
epsilon = 1e-9

approxEq :: Double -> Double -> Bool
approxEq x y = abs (x - y) <= epsilon

pointEq :: (Double, Double) -> (Double, Double) -> Bool
pointEq (x1, y1) (x2, y2) = approxEq x1 x2 && approxEq y1 y2

matrixEq :: Matrix -> Matrix -> Bool
matrixEq m1 m2 =
  let (a1, b1, c1, d1, e1, f1) = components m1
      (a2, b2, c2, d2, e2, f2) = components m2
   in approxEq a1 a2
      && approxEq b1 b2
      && approxEq c1 c2
      && approxEq d1 d2
      && approxEq e1 e2
      && approxEq f1 f2

data Result = Pass String | Fail String

pass :: String -> Result
pass label = Pass label

testFail :: String -> String -> Result
testFail label msg = Fail (label ++ ": FAIL (" ++ msg ++ ")")

assertEq :: String -> Matrix -> Matrix -> Result
assertEq label expected actual =
  if matrixEq expected actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

assertPointEq :: String -> (Double, Double) -> (Double, Double) -> Result
assertPointEq label expected actual =
  if pointEq expected actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

assertBool :: String -> Bool -> Result
assertBool label True = pass label
assertBool label False = testFail label "condition false"

assertDoubleEq :: String -> Double -> Double -> Result
assertDoubleEq label expected actual =
  if approxEq expected actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

assertTextEq :: String -> T.Text -> T.Text -> Result
assertTextEq label expected actual =
  if expected == actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

assertBytesEq :: String -> BS.ByteString -> BS.ByteString -> Result
assertBytesEq label expected actual =
  if expected == actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

assertGlyphOrigin :: String -> Double -> Double -> Glyph -> Result
assertGlyphOrigin label x y g =
  if approxEq x (glyphX g) && approxEq y (glyphY g)
    then pass label
    else testFail label ("expected origin (" ++ show x ++ "," ++ show y ++ "), got (" ++ show (glyphX g) ++ "," ++ show (glyphY g) ++ ")")

assertGlyphWidth :: String -> Double -> Glyph -> Result
assertGlyphWidth label w g =
  assertDoubleEq label w (glyphWidth g)

assertGlyphSize :: String -> Double -> Glyph -> Result
assertGlyphSize label s g =
  assertDoubleEq label s (glyphSize g)

assertMapEq :: String -> M.Map Int Double -> M.Map Int Double -> Result
assertMapEq label expected actual =
  if expected == actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

sampleMatrices :: [Matrix]
sampleMatrices =
  [ identity
  , translate 3 4
  , scale 2 3
  , mkMatrix 1 2 3 4 5 6
  , multiply (scale 12 12) (translate 100 700)
  ]

samplePoints :: [(Double, Double)]
samplePoints =
  [ (0, 0)
  , (1, 0)
  , (0, 1)
  , (1, 1)
  , (5, 7)
  , (-2, 3.5)
  ]

main :: IO ()
main = do
  let results =
        identityLeftResults
          ++ identityRightResults
          ++ applyIdentityResults
          ++ associativityResults
          ++ composeApplyResults
          ++ translateScaleResults
          ++ compoundPdfResults
          ++ cmPremultiplyResults
          ++ parseCIDWidthsResults
          ++ simpleWidthAtResults
          ++ interpretGeometryResults
          ++ layoutParagraphResults
          ++ sortLinesByReadingOrderResults
          ++ layoutDocumentResults
          ++ superscriptResults
          ++ rubyResults
          ++ interpretGraphicResults
          ++ interpretMCIDResults
          ++ structureTreeResults
          ++ pageApiResults
          ++ diffResults
          ++ filterDecodeResults
          ++ imageExtractResults
          ++ formExtractResults
          ++ taggedEndToEndResults
          ++ textStreamResults
          ++ cmapEncodingResults
          ++ normalizePdfNumberResults
          ++ heightSpecResults
          ++ encryptSpecResults
          ++ tuiScrollResults
  let failures = [msg | Fail msg <- results]
      passed = length results - length failures
  mapM_ reportResult results
  putStrLn $ show passed ++ " cases passed"
  when (not (null failures)) exitFailure

reportResult :: Result -> IO ()
reportResult (Pass label) = putStrLn $ label ++ ": PASS"
reportResult (Fail msg) = putStrLn msg

identityLeftResults :: [Result]
identityLeftResults =
  [ assertEq ("multiply identity m: " ++ show m) m (multiply identity m)
  | m <- sampleMatrices
  ]

identityRightResults :: [Result]
identityRightResults =
  [ assertEq ("multiply m identity: " ++ show m) m (multiply m identity)
  | m <- sampleMatrices
  ]

applyIdentityResults :: [Result]
applyIdentityResults =
  [ assertPointEq ("apply identity " ++ show p) p (apply identity p)
  | p <- samplePoints
  ]

associativityMatrices :: (Matrix, Matrix, Matrix)
associativityMatrices =
  ( mkMatrix 1 2 3 4 5 6
  , translate 1 2
  , scale 3 4
  )

associativityResults :: [Result]
associativityResults =
  let (m1, m2, m3) = associativityMatrices
      left = multiply (multiply m1 m2) m3
      right = multiply m1 (multiply m2 m3)
   in [assertEq "associativity multiply (m1*m2)*m3 == m1*(m2*m3)" right left]

composeApplyResults :: [Result]
composeApplyResults =
  [ assertPointEq
      ("apply (multiply m1 m2) " ++ show p ++ " == apply m2 (apply m1 p)")
      (apply m2 (apply m1 p))
      (apply (multiply m1 m2) p)
  | m1 <- sampleMatrices
  , m2 <- sampleMatrices
  , p <- samplePoints
  ]

translateScaleResults :: [Result]
translateScaleResults =
  [ assertPointEq "apply (translate 3 4) (1,1)" (4, 5) (apply (translate 3 4) (1, 1))
  , assertPointEq "apply (scale 2 3) (5,7)" (10, 21) (apply (scale 2 3) (5, 7))
  ]

compoundPdfResults :: [Result]
compoundPdfResults =
  let m = multiply (scale 12 12) (translate 100 700)
   in [ assertPointEq "compound scale*translate at origin" (100, 700) (apply m (0, 0))
      , assertPointEq "compound scale*translate at (1,0)" (112, 700) (apply m (1, 0))
      ]

cmPremultiplyResults :: [Result]
cmPremultiplyResults =
  let cmMat = scale 2 2
      ctm1 = multiply cmMat identity
      textMat = scale 10 10
      trm = multiply (multiply textMat (translate 30 200)) ctm1
   in [ assertPointEq "cm premultiply: multiply cmMat ctm stores scale"
          (0, 0) (apply ctm1 (0, 0))
      , assertPointEq "cm premultiply: scaled text rendering origin"
          (60, 400) (apply trm (0, 0))
      , assertPointEq "cm premultiply: unit x vector length"
          (2, 0) (applyVec ctm1 (1, 0))
      ]

testFontWidth :: Int -> Double
testFontWidth 65 = 600
testFontWidth 66 = 700
testFontWidth 81 = 550
testFontWidth 88 = 500
testFontWidth 89 = 600
testFontWidth _  = 500

testFontInfo :: FontInfo
testFontInfo = FontInfo
  { fiEncoding = NullMap
  , fiToUnicode = M.empty
  , fiWidth = testFontWidth
  , fiWidthV = const (-500)
  , fiWMode = 0
  , fiBytesPerCode = 1
  , fiDefaultWidth = 500
  }

verticalFontInfo :: FontInfo
verticalFontInfo = testFontInfo {fiWMode = 1}

testResources :: M.Map T.Text Obj
testResources = M.fromList [("/Font", PdfDict M.empty)]

testFonts :: M.Map T.Text FontInfo
testFonts = M.fromList [("/F1", testFontInfo)]

runInterp :: String -> [Glyph]
runInterp content =
  interpretContentWithFonts Nothing M.empty testResources testFonts (BSLC.pack content)

runInterpVertical :: String -> [Glyph]
runInterpVertical content =
  interpretContentWithFonts Nothing M.empty testResources (M.singleton "/F1" verticalFontInfo) (BSLC.pack content)

interpretGeometryResults :: [Result]
interpretGeometryResults =
  let ab = runInterp "BT /F1 10 Tf 100 700 Td (AB) Tj ET"
      tmxy = runInterp "BT /F1 10 Tf 1 0 0 1 200 500 Tm (X) Tj ET"
      tj = runInterp "BT /F1 10 Tf 50 400 Td [(A) -200 (B)] TJ ET"
      scaled = runInterp "q 2 0 0 2 0 0 cm BT /F1 10 Tf 30 200 Td (Q) Tj Q ET"
      restore = runInterp "q 2 0 0 2 0 0 cm BT /F1 10 Tf 10 10 Td (A) Tj ET Q BT /F1 10 Tf 10 10 Td (B) Tj ET"
      vert = runInterpVertical "BT /F1 10 Tf 100 200 Td (A) Tj ET"
      leadingDot = runInterp "q .913 0 0 .913 0 595.276 cm BT /F1 10 Tf 0 0 Td (X) Tj ET"
   in [ assertBool "interpret Tj AB produces one segment" (length ab == 1)
      , assertTextEq "interpret Tj AB text" (T.pack "AB") (glyphText (head ab))
      , assertGlyphOrigin "interpret Tj AB origin" 100 700 (head ab)
      , assertGlyphWidth "interpret Tj AB width" 13 (head ab)
      , assertGlyphSize "interpret Tj AB size" 10 (head ab)
      , assertBool "interpret Tm produces one segment" (length tmxy == 1)
      , assertGlyphOrigin "interpret Tm origin" 200 500 (head tmxy)
      , assertGlyphWidth "interpret Tm width" 5 (head tmxy)
      , assertBool "interpret TJ produces two segments" (length tj == 2)
      , assertGlyphOrigin "interpret TJ first origin" 50 400 (tj !! 0)
      , assertGlyphWidth "interpret TJ first width" 6 (tj !! 0)
      , assertGlyphOrigin "interpret TJ second origin" 58 400 (tj !! 1)
      , assertGlyphWidth "interpret TJ second width" 7 (tj !! 1)
      , assertBool "interpret CTM scale produces one segment" (length scaled == 1)
      , assertGlyphOrigin "interpret CTM scale origin" 60 400 (head scaled)
      , assertGlyphSize "interpret CTM scale size" 20 (head scaled)
      , assertGlyphWidth "interpret CTM scale width" 11 (head scaled)
      , assertBool "interpret q/Q restore produces two segments" (length restore == 2)
      , assertGlyphOrigin "interpret q/Q first origin scaled" 20 20 (restore !! 0)
      , assertGlyphOrigin "interpret q/Q second origin restored" 10 10 (restore !! 1)
      , assertBool "interpret vertical produces one segment" (length vert == 1)
      , assertGlyphOrigin "interpret vertical origin" 100 200 (head vert)
      , assertGlyphWidth "interpret vertical width" 5 (head vert)
      , assertBool "interpret hex Tj without space" (length hexTj == 1)
      , assertTextEq "interpret hex Tj text" (T.pack "AB") (glyphText (head hexTj))
      , assertBool "interpret leading-dot cm produces one segment" (length leadingDot == 1)
      , assertGlyphSize "interpret leading-dot cm size" 9.13 (head leadingDot)
      , assertGlyphWidth "interpret leading-dot cm width" 4.565 (head leadingDot)
      ]
  where
    hexTj = runInterp "BT /F1 10 Tf 0 0 Td <4142>Tj ET"

mkGlyph :: Double -> Double -> Double -> Double -> Int -> T.Text -> Glyph
mkGlyph x y w sz wmode txt =
  Glyph
    { glyphText = txt
    , glyphX = x
    , glyphY = y
    , glyphWidth = w
    , glyphSize = sz
    , glyphFont = "/F1"
    , glyphWMode = wmode
    , glyphMCID = Nothing
    }

layoutParagraphResults :: [Result]
layoutParagraphResults =
  let samePara = layoutParagraphs
        [ ItemGlyph (mkGlyph 72 700 100 12 0 "Alpha")
        , ItemGlyph (mkGlyph 72 686 100 12 0 "Beta")
        ]
      gapSplit = layoutParagraphs
        [ ItemGlyph (mkGlyph 72 700 100 12 0 "First")
        , ItemGlyph (mkGlyph 72 650 100 12 0 "Second")
        ]
      indentSplit = layoutParagraphs
        [ ItemGlyph (mkGlyph 72 700 100 12 0 "Normal")
        , ItemGlyph (mkGlyph 92 686 100 12 0 "Indented")
        ]
      ruleSplit = layoutParagraphs
        [ ItemGlyph (mkGlyph 72 700 100 12 0 "Above")
        , ItemGraphic (Rect 70 680 200 682)
        , ItemGlyph (mkGlyph 72 660 100 12 0 "Below")
        ]
      latinSpace = layoutPageText
        [ ItemGlyph (mkGlyph 72 700 6 12 0 "A")
        , ItemGlyph (mkGlyph 85 700 6 12 0 "B")
        ]
      latinLowGap = layoutPageText
        [ ItemGlyph (mkGlyph 72 700 36 9 0 "Word")
        , ItemGlyph (mkGlyph 110.25 700 30 9 0 "Next")
        ]
      latinZeroGap = layoutPageText
        [ ItemGlyph (mkGlyph 72 700 15 9 0 "Wor")
        , ItemGlyph (mkGlyph 87 700 10 9 0 "d")
        ]
      cjkHalfGap = layoutPageText
        [ ItemGlyph (mkGlyph 72 700 12 12 0 "\x3068")
        , ItemGlyph (mkGlyph 90 700 12 12 0 "\x3046")
        ]
      hyphenJoin = head (layoutParagraphs
        [ ItemGlyph (mkGlyph 72 511.83 60 10 0 "authenti-")
        , ItemGlyph (mkGlyph 72 496.527 40 10 0 "cated")
        ])
      cjkNoSpace = layoutPageText
        [ ItemGlyph (mkGlyph 72 700 12 12 0 "\x3068")
        , ItemGlyph (mkGlyph 84 700 12 12 0 "\x3046")
        ]
      fallbackZero = layoutParagraphs
        [ ItemGlyph (mkGlyph 72 700 10 0 0 "X")
        , ItemGlyph (mkGlyph 72 686 10 0 0 "Y")
        ]
      vertCols = layoutParagraphs
        [ ItemGlyph (mkGlyph 500 700 10 12 1 "\x3042")
        , ItemGlyph (mkGlyph 500 680 10 12 1 "\x3044")
        , ItemGlyph (mkGlyph 440 700 10 12 1 "\x3046")
        , ItemGlyph (mkGlyph 440 680 10 12 1 "\x3048")
        ]
      outlierSplit = layoutParagraphs
        [ ItemGlyph (mkGlyph 70 384 8 8 0 "\x5b9f")
        , ItemGlyph (mkGlyph 255 387 4 6 0 "\x2020")
        , ItemGlyph (mkGlyph 259 (-167) 3 5 0 "2")
        , ItemGlyph (mkGlyph 263 384 8 8 0 "\x3092")
        , ItemGlyph (mkGlyph 310 384 8 8 0 "\x8a18")
        , ItemGlyph (mkGlyph 62 372 8 8 0 "\x6cd5")
        , ItemGlyph (mkGlyph 70 372 8 8 0 "\x3067")
        ]
      cjkWrapSplit = layoutParagraphs
        [ ItemGlyph (mkGlyph 310 384 8 8 0 "\x8a18")
        , ItemGlyph (mkGlyph 62 372 8 8 0 "\x6cd5")
        , ItemGlyph (mkGlyph 70 372 8 8 0 "\x3067")
        , ItemGlyph (mkGlyph 78 372 8 8 0 "\x898b")
        ]
      dingbatBullet = layoutPageText
        [ ItemGlyph (mkGlyph 60 434 0 9 0 "r")
        , ItemGlyph (mkGlyph 66 431 0 8 0 "HTTP")
        ]
      letteredList = layoutParagraphs
        [ ItemGlyph (mkGlyph 63 275 6 8 0 "a.")
        , ItemGlyph (mkGlyph 74 275 40 8 0 "\x533a\x5225")
        , ItemGlyph (mkGlyph 63 260 6 8 0 "b.")
        , ItemGlyph (mkGlyph 70 260 40 8 0 "\x30bd\x30fc\x30b9")
        , ItemGlyph (mkGlyph 58 241 40 8 0 "\x4fe1\x983c")
        ]
      hangList = layoutParagraphs
        [ ItemGlyph (mkGlyph 66 201 80 8 0 "\x5909\x6570\x306f\x3053\x3068")
        , ItemGlyph (mkGlyph 66 186 80 8 0 "\x30bd\x30fc\x30b9\x3053\x3068")
        , ItemGlyph (mkGlyph 66 171 80 8 0 "\x30b9\x30ad\x30fc\x30de\x3053\x3068")
        ]
      codeBlockText = layoutPageText
        [ ItemGlyph (mkGlyph 274 511 10 6 0 "\x30b3\x30fc\x30c9")
        , ItemGlyph (mkGlyph 40 506 4 6 0 "1")
        , ItemGlyph (mkGlyph 55 506 20 6 0 "is.into_iter()")
        , ItemGlyph (mkGlyph 40 499 4 6 0 "2")
        , ItemGlyph (mkGlyph 70 499 20 6 0 ".flatten()")
        ]
   in [ assertBool "layout same-baseline two lines one paragraph" (length samePara == 1)
      , assertTextEq "layout same-baseline joined"
          (T.pack "Alpha Beta") (head samePara)
      , assertBool "layout large gap two paragraphs" (length gapSplit == 2)
      , assertBool "layout indent starts new paragraph" (length indentSplit == 2)
      , assertBool "layout graphic rule splits paragraphs" (length ruleSplit == 2)
      , assertTextEq "layout Latin gap inserts space" (T.pack "A B\n") latinSpace
      , assertTextEq "layout Latin low gap inserts space" (T.pack "Word Next\n") latinLowGap
      , assertTextEq "layout Latin zero gap no space" (T.pack "Word\n") latinZeroGap
      , assertTextEq "layout CJK half-size gap no space" (T.pack "\x3068\x3046\n") cjkHalfGap
      , assertTextEq "layout hyphen line join" (T.pack "authenti-cated") hyphenJoin
      , assertBool "layout hyphen line join no spurious space"
          (not (T.isInfixOf "authenti- cated" hyphenJoin))
      , assertTextEq "layout CJK gap no space" (T.pack "\x3068\x3046\n") cjkNoSpace
      , assertBool "layout fallback zero sizes" (length fallbackZero == 1)
      , assertTextEq "layout fallback joins with newline"
          (T.pack "X\nY") (head fallbackZero)
      , assertBool "layout vertical two columns" (length vertCols == 2)
      , assertTextEq "layout vertical first column"
          (T.pack "\x3042\x3044") (head vertCols)
      , assertTextEq "layout vertical second column"
          (T.pack "\x3046\x3048") (vertCols !! 1)
      , assertBool "layout outlier glyph does not split paragraph" (length outlierSplit == 1)
      , assertBool "layout outlier glyph keeps CJK wrap"
          (T.isInfixOf (T.pack "\x8a18\x6cd5\x3067") (head outlierSplit))
      , assertBool "layout CJK wrap keeps one paragraph" (length cjkWrapSplit == 1)
      , assertTextEq "layout CJK wrap joined text"
          (T.pack "\x8a18\x6cd5\x3067\x898b") (head cjkWrapSplit)
      , assertTextEq "layout dingbat r prefix becomes bullet"
          (T.pack "\8226 HTTP\n") dingbatBullet
      , assertBool "layout lettered list items split" (length letteredList == 3)
      , assertBool "layout hang-indent list items split" (length hangList == 3)
      , assertBool "layout code block keeps newlines"
          (length (T.lines codeBlockText) >= 3)
      , assertBool "layout code caption separate from block"
          (length (layoutParagraphs
            [ ItemGlyph (mkGlyph 274 511 10 6 0 "\x30b3\x30fc\x30c9")
            , ItemGlyph (mkGlyph 40 506 4 6 0 "1")
            , ItemGlyph (mkGlyph 55 506 20 6 0 "is")
            ]) == 2)
      ]

sortLinesByReadingOrderResults :: [Result]
sortLinesByReadingOrderResults =
  let rowOrderItems =
        [ ItemGlyph (mkGlyph 72 650 30 12 0 "Bot-C")
        , ItemGlyph (mkGlyph 72 700 30 12 0 "Top-A")
        , ItemGlyph (mkGlyph 200 700 30 12 0 "Top-B")
        ]
      rowOrderText = layoutPageText rowOrderItems
      vertReversedItems =
        [ ItemGlyph (mkGlyph 440 700 10 12 1 "\x3046")
        , ItemGlyph (mkGlyph 440 680 10 12 1 "\x3048")
        , ItemGlyph (mkGlyph 500 700 10 12 1 "\x3042")
        , ItemGlyph (mkGlyph 500 680 10 12 1 "\x3044")
        ]
      vertReversed = layoutParagraphs vertReversedItems
      sameRowLines =
        [ mkTestLine 700 72 78 0 "A"
        , mkTestLine 700 200 206 0 "B"
        ]
      sortedSameRow = map lineText (sortLinesByReadingOrder sameRowLines)
   in [ assertBool "sort rows: Top before Bot"
          (let t = T.stripEnd rowOrderText
               (preA, _) = T.breakOn "Top-A" t
               (preB, _) = T.breakOn "Bot-C" t
           in T.isInfixOf "Top-A" t
              && T.isInfixOf "Bot-C" t
              && T.length preA <= T.length preB)
      , assertTextEq "sort same row left-to-right"
          (T.pack "A B") (T.intercalate " " sortedSameRow)
      , assertBool "sort vertical columns right-to-left"
          (length vertReversed == 2
           && head vertReversed == T.pack "\x3042\x3044"
           && vertReversed !! 1 == T.pack "\x3046\x3048")
      , assertTextEq "sortLinesByReadingOrder direct"
          (T.pack "A B") (T.intercalate " " sortedSameRow)
      ]

mkTestLine :: Double -> Double -> Double -> Int -> T.Text -> Line
mkTestLine baseline inlineStart inlineEnd wmode txt =
  Line
    { lineBaseline = baseline
    , lineInlineStart = inlineStart
    , lineInlineEnd = inlineEnd
    , lineSize = 12
    , lineFirstInline = inlineStart
    , lineWMode = wmode
    , lineText = txt
    , lineMarkers = []
    , lineLastSuper = False
    }

headerBodyFooter :: Int -> T.Text -> T.Text -> [PageItem]
headerBodyFooter n header body =
  [ ItemGlyph (mkGlyph 250 750 80 12 0 header)
  , ItemGlyph (mkGlyph 72 700 100 12 0 body)
  , ItemGlyph (mkGlyph 280 50 10 12 0 (T.pack (show n)))
  ]

layoutDocumentResults :: [Result]
layoutDocumentResults =
  let repeatedHdr =
        layoutDocument
          [ headerBodyFooter 1 "RepeatHeader" "Body one."
          , headerBodyFooter 2 "RepeatHeader" "Body two."
          , headerBodyFooter 3 "RepeatHeader" "Body three."
          ]
      pageNums =
        layoutDocument
          [ headerBodyFooter 1 "UniqueA" "Alpha text."
          , headerBodyFooter 2 "UniqueB" "Beta text."
          ]
      uniqueHdr =
        layoutDocument
          [ headerBodyFooter 1 "HdrA" "One."
          , headerBodyFooter 2 "HdrB" "Two."
          , headerBodyFooter 3 "HdrC" "Three."
          ]
      cjkMerge =
        layoutDocument
          [ [ ItemGlyph (mkGlyph 72 700 12 12 0 "\x6587\x3068")
            ]
          , [ ItemGlyph (mkGlyph 72 700 12 12 0 "\x3046\x308b")
            ]
          ]
      terminalSplit =
        layoutDocument
          [ [ ItemGlyph (mkGlyph 72 700 12 12 0 "\x7d42\x308f\x308a\x3002")
            ]
          , [ ItemGlyph (mkGlyph 72 700 12 12 0 "\x59cb\x307e\x308a")
            ]
          ]
      indentPageSplit =
        layoutDocument
          [ [ ItemGlyph (mkGlyph 72 700 100 12 0 "First part")
            ]
          , [ ItemGlyph (mkGlyph 84 700 100 12 0 "Indented start")
            , ItemGlyph (mkGlyph 72 680 100 12 0 "Same page body")
            ]
          ]
      indent1em =
        layoutParagraphs
          [ ItemGlyph (mkGlyph 72 700 100 12 0 "Normal")
          , ItemGlyph (mkGlyph 84 686 100 12 0 "Indented")
          ]
      titlePage =
        layoutDocument
          [ [ ItemGlyph (mkGlyph 250 750 80 12 0 "Unique Title Header")
            , ItemGlyph (mkGlyph 250 400 80 12 0 "Book Title")
            ]
          , headerBodyFooter 2 "RepeatHeader" "Chapter one."
          , headerBodyFooter 3 "RepeatHeader" "Chapter two."
          ]
      twoLineRepeated =
        layoutDocument
          (replicate 10
             [ ItemGlyph (mkGlyph 250 750 80 12 0 "RepeatHeader")
             , ItemGlyph (mkGlyph 72 700 100 12 0 "Body only.")
             ])
      alternatingHdr =
        layoutDocument
          (zipWith
             (\hdr body -> headerBodyFooter 1 hdr body)
             (cycle ["vi 序文", "序文 vii"])
             (replicate 10 "Body text."))
   in [ assertBool "layout doc repeated header removed"
          (not (T.isInfixOf "RepeatHeader" repeatedHdr))
      , assertBool "layout doc bare page numbers removed"
          (not (any (\n -> T.strip n `elem` [T.pack "1", T.pack "2"]) (T.lines pageNums)))
      , assertBool "layout doc unique headers kept"
          (T.isInfixOf "HdrA" uniqueHdr
           && T.isInfixOf "HdrB" uniqueHdr
           && T.isInfixOf "HdrC" uniqueHdr)
      , assertTextEq "layout doc CJK cross-page merge"
          (T.pack "\x6587\x3068\x3046\x308b\n")
          cjkMerge
      , assertBool "layout doc terminal punctuation splits pages"
          (length (T.lines (T.stripEnd terminalSplit)) >= 2)
      , assertBool "layout doc indented page start splits"
          (length (T.lines (T.stripEnd indentPageSplit)) >= 2)
      , assertBool "layout doc indent break at 1.0em" (length indent1em == 2)
      , assertBool "layout doc title page keeps all lines"
          (T.isInfixOf "Unique Title Header" titlePage
           && T.isInfixOf "Book Title" titlePage)
      , assertBool "layout doc two-line page repeated header removed"
          (not (T.isInfixOf "RepeatHeader" twoLineRepeated))
      , assertBool "layout doc alternating roman headers removed"
          (not (T.isInfixOf "vi 序文" alternatingHdr)
           && not (T.isInfixOf "序文 vii" alternatingHdr))
      ]

-- A body line at y=700 size 10, with a superscript marker (size 7,
-- +3.5 above) and footnote apparatus at page bottom.
superscriptResults :: [Result]
superscriptResults =
  let body t = ItemGlyph (mkGlyph 72 700 60 10 0 t)
      supAt x t = ItemGlyph (mkGlyph x 703.5 4 7 0 t)
      bodyAfter x t = ItemGlyph (mkGlyph x 700 10 10 0 t)
      merged = layoutParagraphs
        [ body "text", supAt 132 "\8224", supAt 136 "1", bodyAfter 140 "." ]
      farSupGlyphs =
        [ g | ItemGlyph g <- [body "text", ItemGlyph (mkGlyph 132 708.5 4 7 0 "\8224")]]
      rebase = layoutParagraphs
        [ ItemGlyph (mkGlyph 72 87.6 4 7 0 "\8224")
        , ItemGlyph (mkGlyph 76 87.6 4 7 0 "1")
        , ItemGlyph (mkGlyph 82 84.3 100 9 0 "note body")
        ]
      fnPage =
        [ body "anchor", supAt 132 "\8224", supAt 136 "1"
        , ItemGlyph (mkGlyph 72 500 60 10 0 "more body text here")
        , ItemGraphic (Rect 72 95 200 95.5)
        , ItemGlyph (mkGlyph 72 87.6 3 7 0 "\8224")
        , ItemGlyph (mkGlyph 75 87.6 3 7 0 "1")
        , ItemGlyph (mkGlyph 80 84.3 60 8 0 "the note")
        ]
      fnOn = T.concat (layoutParagraphsWith (defaultLayoutOptions {optFootnotes = True}) fnPage)
      fnOff = T.concat (layoutParagraphs fnPage)
      unmatchedAnchor = T.concat
        (layoutParagraphsWith (defaultLayoutOptions {optFootnotes = True})
          [ body "anchor", supAt 132 "\8224", supAt 136 "9"
          , ItemGlyph (mkGlyph 72 500 60 10 0 "more body text here")
          ])
      orphanBlock = T.concat
        (layoutParagraphsWith (defaultLayoutOptions {optFootnotes = True})
          [ body "just body content"
          , ItemGlyph (mkGlyph 72 500 60 10 0 "more body text here")
          , ItemGlyph (mkGlyph 72 87.6 3 7 0 "\8224")
          , ItemGlyph (mkGlyph 75 87.6 3 7 0 "2")
          , ItemGlyph (mkGlyph 80 84.3 60 8 0 "orphan note")
          ])
   in [ assertBool "superscript merges inline" (length merged == 1)
      , assertTextEq "superscript inline text order"
          (T.pack "text\8224\&1.") (head merged)
      , assertBool "superscript beyond window stays separate line"
          (length (linesFromGlyphs farSupGlyphs) == 2)
      , assertBool "marker line rebases onto body" (length rebase == 1)
      , assertTextEq "rebased line text" (T.pack "\8224\&1note body") (head rebase)
      , assertBool "footnotes on: body inlined"
          (T.isInfixOf "anchor<footnote>the note</footnote>" fnOn)
      , assertBool "footnotes on: block removed"
          (not (T.isInfixOf "\8224\&1 the note" fnOn))
      , assertBool "footnotes off: marker kept"
          (T.isInfixOf "anchor\8224\&1" fnOff)
      , assertBool "footnotes off: block kept"
          (T.isInfixOf "the note" fnOff)
      , assertBool "unmatched anchor kept as-is"
          (T.isInfixOf "anchor\8224\&9" unmatchedAnchor)
      , assertBool "orphan footnote block stays"
          (T.isInfixOf "orphan note" orphanBlock)
      ]

rubyResults :: [Result]
rubyResults =
  let kanji = "\x6f22"  -- 漢
      kanRuby = "\x304b\x3093"  -- かん
      mixedBase = "\x9727\x306e\x30ed\x30f3\x30c9\x30f3\x8b66\x8996\x5e81"
      mixedRuby = "\x30b9\x30b3\x30c3\x30c8\x30e9\x30f3\x30c9\x30e4\x30fc\x30c9"
      horizPage =
        [ ItemGlyph (mkGlyph 72 700 12 12 0 kanji)
        , ItemGlyph (mkGlyph 72 710 6 7 0 "\x304b")
        , ItemGlyph (mkGlyph 78 710 6 7 0 "\x3093")
        ]
      vertPage =
        [ ItemGlyph (mkGlyph 500 700 12 12 1 kanji)
        , ItemGlyph (mkGlyph 488 700 14 7 1 kanRuby)
        ]
      mixedPage =
        [ ItemGlyph (mkGlyph 72 700 120 12 0 mixedBase)
        , ItemGlyph (mkGlyph 72 710 80 7 0 mixedRuby)
        ]
      interleavedPage =
        let bodyY = 110.85098199999993
            rubyY = 118.75664899999992
            bodySz = 8.410829900000001
            rubySz = 4.2053693
        in [ ItemGlyph (mkGlyph 66.33 bodyY 100.93 bodySz 0 "\x64cd\4f5c\3092")
           , ItemGlyph (mkGlyph 167.26 rubyY 8.41 rubySz 0 "\x3079\x304d")
           , ItemGlyph (mkGlyph 167.26 bodyY 8.41 bodySz 0 "\x5186")
           , ItemGlyph (mkGlyph 175.67 rubyY 8.41 rubySz 0 "\x3068\x3046")
           , ItemGlyph (mkGlyph 175.67 bodyY 84.11 bodySz 0 "\x7b49\306a\3082\306e\306b\3059\308b\3053\3068\12290")
           ]
      rubyOn = layoutParagraphsWith (defaultLayoutOptions {optRuby = True})
      rubyOff = layoutParagraphsWith defaultLayoutOptions
      horizOn = T.concat (rubyOn horizPage)
      horizOff = T.concat (rubyOff horizPage)
      vertOn = T.concat (rubyOn vertPage)
      mixedOn = T.concat (rubyOn mixedPage)
   in [ assertBool "needsAozoraBar single hiragana" (not (needsAozoraBar "\x3042\x304a\x305e\x3089"))
      , assertBool "needsAozoraBar single kanji" (not (needsAozoraBar "\x9752\x7a7a\x6587\x5eab"))
      , assertBool "needsAozoraBar mixed scripts" (needsAozoraBar mixedBase)
      , assertTextEq "aozoraRuby single script"
          (kanji `T.append` "\x300a" `T.append` kanRuby `T.append` "\x300b")
          (aozoraRuby kanji kanRuby)
      , assertTextEq "aozoraRuby mixed base bar"
          (mixedBase `T.append` "\xff5c\x300a" `T.append` mixedRuby `T.append` "\x300b")
          (aozoraRuby mixedBase mixedRuby)
      , assertBool "ruby horizontal geometry"
          (T.isInfixOf (kanji `T.append` "\x300a" `T.append` kanRuby `T.append` "\x300b") horizOn)
      , assertBool "ruby vertical geometry"
          (T.isInfixOf (kanji `T.append` "\x300a" `T.append` kanRuby `T.append` "\x300b") vertOn)
      , assertBool "ruby mixed base bar in layout"
          (T.isInfixOf ("\xff5c\x300a" `T.append` mixedRuby `T.append` "\x300b") mixedOn)
      , assertBool "ruby off leaves plain text"
          (not (T.isInfixOf "\x300a" horizOff) && T.isInfixOf kanji horizOff)
      , assertBool "ruby interleaved stream order"
          (T.isInfixOf
            ("\x5186\x7b49\x300a\x3079\x304d\x3068\x3046\x300b")
            (T.concat (rubyOn interleavedPage)))
      , assertBool "ruby interleaved off suppresses orphan lines"
          (let out = T.concat (rubyOff interleavedPage)
           in T.isInfixOf "\x5186\x7b49" out
              && not (T.isInfixOf "\x3079\x304d" out)
              && not (T.isInfixOf "\x3068\x3046" out))
      ]

interpretGraphicResults :: [Result]
interpretGraphicResults =
  let items = interpretContentWithFontsItems Nothing M.empty testResources testFonts
          (BSLC.pack "10 0 0 10 50 100 cm 0 0 20 10 re f")
      graphics = [r | ItemGraphic r <- items]
   in [ assertBool "interpret re f emits one graphic" (length graphics == 1)
      , assertRectEq "interpret re f bbox x0" 50 (rectX0 (head graphics))
      , assertRectEq "interpret re f bbox y0" 100 (rectY0 (head graphics))
      , assertRectEq "interpret re f bbox x1" 250 (rectX1 (head graphics))
      , assertRectEq "interpret re f bbox y1" 200 (rectY1 (head graphics))
      ]

assertRectEq :: String -> Double -> Double -> Result
assertRectEq label expected actual = assertDoubleEq label expected actual

parseCIDWidthsResults :: [Result]
parseCIDWidthsResults =
  [ assertMapEq "parseCIDWidths consecutive form"
      (M.fromList [(10, 100), (11, 200), (12, 300)])
      (parseCIDWidths
        [ PdfNumber 10
        , PdfArray [PdfNumber 100, PdfNumber 200, PdfNumber 300]
        ])
  , assertMapEq "parseCIDWidths range form"
      (M.fromList [(5, 500), (6, 500), (7, 500)])
      (parseCIDWidths [PdfNumber 5, PdfNumber 7, PdfNumber 500])
  , assertMapEq "parseCIDWidths mixed forms"
      (M.fromList [(1, 50), (2, 50), (10, 100), (11, 200)])
      (parseCIDWidths
        [ PdfNumber 1, PdfNumber 2, PdfNumber 50
        , PdfNumber 10
        , PdfArray [PdfNumber 100, PdfNumber 200]
        ])
  ]

simpleWidthAtResults :: [Result]
simpleWidthAtResults =
  let widths = [PdfNumber 250, PdfNumber 500, PdfNumber 750]
   in [ assertDoubleEq "simpleWidthAt in range" 500 (simpleWidthAt 32 widths 0 33)
      , assertDoubleEq "simpleWidthAt below range" 0 (simpleWidthAt 32 widths 0 20)
      , assertDoubleEq "simpleWidthAt above range" 0 (simpleWidthAt 32 widths 0 40)
      , assertDoubleEq "simpleWidthAt missing width fallback" 99 (simpleWidthAt 0 [PdfText "x"] 99 0)
      ]

interpretMCIDResults :: [Result]
interpretMCIDResults =
  let bdc = runInterp "/P <</MCID 0>> BDC BT /F1 10 Tf 0 0 Td (A) Tj ET EMC"
      nested = runInterp "/Span BMC /P <</MCID 1>> BDC BT /F1 10 Tf 10 0 Td (B) Tj ET EMC EMC"
      inherit = runInterp "/Span BMC BT /F1 10 Tf 0 0 Td (C) Tj ET EMC"
      afterPop = runInterp "/P <</MCID 0>> BDC BT /F1 10 Tf 0 0 Td (D) Tj ET EMC BT /F1 10 Tf 20 0 Td (E) Tj ET"
      underflow = runInterp "BT /F1 10 Tf 0 0 Td (F) Tj ET EMC"
   in [ assertBool "interpret BDC MCID one glyph" (length bdc == 1)
      , assertBool "interpret BDC MCID value" (glyphMCID (head bdc) == Just 0)
      , assertBool "interpret nested BMC inherits MCID" (length nested == 1)
      , assertBool "interpret nested MCID value" (glyphMCID (head nested) == Just 1)
      , assertBool "interpret BMC no MCID on glyph" (all (== Nothing) (map glyphMCID inherit))
      , assertBool "interpret EMC clears MCID" (glyphMCID (afterPop !! 1) == Nothing)
      , assertBool "interpret EMC underflow tolerated" (length underflow == 1)
      ]

mkTestDoc :: M.Map Int [Obj] -> Document
mkTestDoc objs =
  Document (M.fromList [("/Root", ObjRef 1)]) objs Nothing M.empty M.empty

structureTreeResults :: [Result]
structureTreeResults =
  let objs = M.fromList
        [ (1, [PdfDict (M.fromList
            [ ("/Type", PdfName "/Catalog")
            , ("/StructTreeRoot", ObjRef 6)
            ])])
        , (6, [PdfDict (M.fromList
            [ ("/Type", PdfName "/StructTreeRoot")
            , ("/K", ObjRef 8)
            ])])
        , (8, [PdfDict (M.fromList
            [ ("/Type", PdfName "/StructElem")
            , ("/S", PdfName "/Document")
            , ("/K", PdfArray [ObjRef 9, PdfNumber 2, ObjRef 10, ObjRef 11])
            ])])
        , (9, [PdfDict (M.fromList
            [ ("/Type", PdfName "/StructElem")
            , ("/S", PdfName "/Sect")
            , ("/Pg", ObjRef 3)
            , ("/K", PdfNumber 0)
            ])])
        , (10, [PdfDict (M.fromList
            [ ("/Type", PdfName "/MCR")
            , ("/Pg", ObjRef 3)
            , ("/MCID", PdfNumber 1)
            ])])
        , (11, [PdfDict (M.fromList
            [ ("/Type", PdfName "/OBJR")
            , ("/Obj", ObjRef 99)
            ])])
        ]
      doc = mkTestDoc objs
      parsed = structTree doc :: PdfResult (Maybe StructElem)
      order = case parsed of
        Right (Just root) -> logicalOrder root
        _ -> []
   in [ assertBool "structTree parses hand-built tree" (isRight parsed)
      , assertBool "structTree root type" (rootType parsed == "/StructTreeRoot")
      , assertBool "structTree nested sect kid" (hasSect parsed)
      , assertBool "structTree K int becomes MCID" (elem (3, 0) (mcidPairs order))
      , assertBool "structTree MCR dict becomes MCID" (elem (3, 1) (mcidPairs order))
      , assertBool "structTree OBJR skipped" (length order == 2)
      ]
  where
    isRight (Right _) = True
    isRight _ = False
    rootType (Right (Just (StructElem t _))) = t
    rootType _ = ""
    hasSect (Right (Just root)) = "/Sect" `elem` elemTypes root
    hasSect _ = False
    elemTypes (StructElem t kids) = t : concatMap kidTypes kids
    kidTypes (KidElem e) = elemTypes e
    kidTypes _ = []
    mcidPairs = map (\(_, page, mcid) -> (page, mcid))

taggedEndToEndResults :: [Result]
taggedEndToEndResults =
  [ runTaggedFixture
  ]

pageApiResults :: [Result]
pageApiResults =
  [ runMultipagePageCount
  , runMultipagePageRefAt
  , runParagraphsPageParagraphs
  ]

runMultipagePageCount :: Result
runMultipagePageCount =
  let path = "data/fixtures/multipage.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case pageCount doc of
          Right n -> assertEqInt "multipage.pdf pageCount" 3 n
          Left err -> testFail "multipage.pdf pageCount" (show err)
      Left err -> testFail "multipage.pdf pageCount open" (show err)

runMultipagePageRefAt :: Result
runMultipagePageRefAt =
  let path = "data/fixtures/multipage.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case pageRefAt doc 1 of
          Right r1 ->
            case pageRefAt doc 3 of
              Right r3 ->
                assertBool "multipage.pdf pageRefAt refs distinct"
                  (r1 /= r3 && r1 == 3 && r3 == 5)
              Left err -> testFail "multipage.pdf pageRefAt page 3" (show err)
          Left err -> testFail "multipage.pdf pageRefAt page 1" (show err)
      Left err -> testFail "multipage.pdf pageRefAt open" (show err)

runParagraphsPageParagraphs :: Result
runParagraphsPageParagraphs =
  let path = "data/fixtures/paragraphs.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case pageRefAt doc 1 of
          Right ref ->
            case pageParagraphs doc ref defaultLayoutOptions of
              Right ps ->
                assertBool "paragraphs.pdf pageParagraphs count"
                  (length ps == 3
                   && T.isInfixOf "Line one of A." (head ps)
                   && T.isInfixOf "Indent line B1." (ps !! 1)
                   && T.strip (last ps) == "Line C only.")
              Left err -> testFail "paragraphs.pdf pageParagraphs" (show err)
          Left err -> testFail "paragraphs.pdf pageRefAt" (show err)
      Left err -> testFail "paragraphs.pdf pageParagraphs open" (show err)

assertEqInt :: String -> Int -> Int -> Result
assertEqInt label expected actual =
  if expected == actual
    then pass label
    else testFail label ("expected " ++ show expected ++ ", got " ++ show actual)

{-# NOINLINE runMultipagePageCount #-}
{-# NOINLINE runMultipagePageRefAt #-}
{-# NOINLINE runParagraphsPageParagraphs #-}

diffResults :: [Result]
diffResults =
  [ assertBool "diffParagraphs identical lists" (null (diffParagraphs ps ps))
  , assertBool "diffParagraphs one change"
      ( case diffParagraphs [T.pack "First paragraph text", T.pack "Second"]
              [T.pack "First paragraph changed", T.pack "Second"] of
          [TextChange{changeParaA = Just 0, changeParaB = Just 0, changeOld = old, changeNew = new}] ->
            old == T.pack "First paragraph text"
              && new == T.pack "First paragraph changed"
          _ -> False
      )
  , assertBool "diffParagraphs insert only"
      ( case diffParagraphs [T.pack "A"] [T.pack "A", T.pack "B"] of
          [TextChange{changeParaB = Just 1, changeOld = old, changeNew = new}] ->
            T.null old && new == T.pack "B"
          _ -> False
      )
  , assertBool "diffParagraphs delete only"
      ( case diffParagraphs [T.pack "A", T.pack "B"] [T.pack "A"] of
          [TextChange{changeParaA = Just 1, changeOld = old, changeNew = new}] ->
            old == T.pack "B" && T.null new
          _ -> False
      )
  , runMultipageSelfDiff
  ]
  where
    ps = [T.pack "Alpha", T.pack "Beta"]

runMultipageSelfDiff :: Result
runMultipageSelfDiff =
  let path = "data/fixtures/multipage.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case compareDocuments defaultLayoutOptions doc doc of
          Right changes -> assertBool "multipage.pdf self-diff empty" (null changes)
          Left err -> testFail "multipage.pdf self-diff" (show err)
      Left err -> testFail "multipage.pdf self-diff open" (show err)

{-# NOINLINE runMultipageSelfDiff #-}

filterDecodeResults :: [Result]
filterDecodeResults =
  [ assertBool "DCTDecode pass-through"
      ( case decodeStreamBytes dctDict (BSLC.pack jpegStub) of
          Right bs -> bs == BSC.pack jpegStub
          _ -> False
      )
  , assertBool "no filter pass-through"
      ( case decodeStreamBytes noFilterDict (BSLC.pack "plain") of
          Right bs -> bs == BSC.pack "plain"
          _ -> False
      )
  ]
  where
    dctDict = M.fromList [("/Filter", PdfName "/DCTDecode")]
    noFilterDict = M.empty
    jpegStub = "\xff\xd8\xff\xe0" ++ replicate 10 'x' ++ "\xff\xd9"

imageExtractResults :: [Result]
imageExtractResults =
  pngEncodeResults
    ++ classifyImageResults
    ++ pageImageFixtureResults

pngEncodeResults :: [Result]
pngEncodeResults =
  let w = 2
      h = 2
      rgb = BS.pack (map (fromIntegral :: Int -> Word8) [255, 0, 0, 0, 255, 0, 0, 0, 255, 255, 255, 255])
      pngSig = BS.pack (map (fromIntegral :: Int -> Word8) [137, 80, 78, 71, 13, 10, 26, 10])
   in [ assertBool "encodePngRgb signature"
          ( case encodePngRgb w h rgb of
              Right png -> BS.take 8 png == pngSig
              _ -> False
          )
      , assertBool "encodePngRgb IHDR chunk"
          ( case encodePngRgb w h rgb of
              Right png -> BS.length png > 40
              _ -> False
          )
      ]

classifyImageResults :: [Result]
classifyImageResults =
  let dctDict = M.fromList [("/Filter", PdfName "/DCTDecode")]
      jpegBs = BSC.pack jpegStub
   in [ assertBool "classifyImageBytes DCTDecode"
          ( case classifyImageBytes M.empty dctDict jpegBs of
              Right (ImageJPEG, bs) -> bs == jpegBs
              _ -> False
          )
      , assertBool "classifyImageBytes raw RGB to PNG"
          ( case classifyImageBytes M.empty rgbDict (BS.replicate 12 0) of
              Right (ImagePNG, png) ->
                BS.take 8 png
                  == BS.pack (map (fromIntegral :: Int -> Word8) [137, 80, 78, 71, 13, 10, 26, 10])
              _ -> False
          )
      ]
  where
    rgbDict =
      M.fromList
        [ ("/Width", PdfNumber 2)
        , ("/Height", PdfNumber 2)
        , ("/BitsPerComponent", PdfNumber 8)
        , ("/ColorSpace", PdfName "/DeviceRGB")
        ]
    jpegStub = "\xff\xd8\xff\xe0" ++ replicate 10 'x' ++ "\xff\xd9"

pageImageFixtureResults :: [Result]
pageImageFixtureResults =
  [ runJpegImageFixture
  , runJpegImageHits
  , runFormNestedImageHits
  ]

runJpegImageFixture :: Result
runJpegImageFixture =
  let path = "data/fixtures/jpeg-image.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case extractPageImages doc 1 of
          Right [img]
            | piFormat img == ImageJPEG
                && BS.take 2 (piBytes img) == BS.pack [0xff, 0xd8] ->
                pass "jpeg-image.pdf extractPageImages"
          Right xs ->
            testFail "jpeg-image.pdf extractPageImages" ("expected 1 image, got " ++ show (length xs))
          Left err -> testFail "jpeg-image.pdf extractPageImages" (show err)
      Left err -> testFail "jpeg-image.pdf open" (show err)

runJpegImageHits :: Result
runJpegImageHits =
  let path = "data/fixtures/jpeg-image.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case pageRefAt doc 1 of
          Right r ->
            case interpretPageImageHits doc r of
              Right [(ref, bbox)]
                | ref == 5 && approxEq 50 (rectX0 bbox) ->
                    pass "jpeg-image interpretPageImageHits"
              Right xs -> testFail "jpeg-image hits" ("unexpected hits " ++ show xs)
              Left err -> testFail "jpeg-image hits" (show err)
          Left err -> testFail "jpeg-image pageRefAt" (show err)
      Left err -> testFail "jpeg-image open hits" (show err)

{-# NOINLINE runJpegImageFixture #-}
{-# NOINLINE runJpegImageHits #-}

runFormNestedImageHits :: Result
runFormNestedImageHits =
  let path = "test/fixtures/form-nested-image.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case pageRefAt doc 1 of
          Right r ->
            case interpretPageImageHits doc r of
              Right [(ref, bbox)]
                | ref == 5 && approxEq 50 (rectX0 bbox) ->
                    pass "form-nested-image interpretPageImageHits"
              Right xs -> testFail "form-nested-image hits" ("unexpected hits " ++ show xs)
              Left err -> testFail "form-nested-image hits" (show err)
          Left err -> testFail "form-nested-image pageRefAt" (show err)
      Left err -> testFail "form-nested-image open hits" (show err)

{-# NOINLINE runFormNestedImageHits #-}

formExtractResults :: [Result]
formExtractResults =
  [ runFormExportParentNames
  , runFormExportParentExtract
  , runFm42Integration
  ]

runFormExportParentNames :: Result
runFormExportParentNames =
  let path = "test/fixtures/form-export-parent.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    return $ case result of
      Right doc ->
        case pageFormNames doc 1 of
          Right names ->
            assertBool "form-export-parent pageFormNames"
              (names == [T.pack "Fm0"])
          Left err -> testFail "form-export-parent pageFormNames" (show err)
      Left err -> testFail "form-export-parent open names" (show err)

runFormExportParentExtract :: Result
runFormExportParentExtract =
  let path = "test/fixtures/form-export-parent.pdf"
  in unsafePerformIO $ do
    result <- openDocument path Nothing
    case result of
      Right doc ->
        case extractFormPdf doc 1 (T.pack "Fm0") of
          Right bytes
            | BSC.take 4 bytes /= BSC.pack "%PDF" ->
                return $ testFail "form-export-parent extractFormPdf" "missing %PDF header"
            | otherwise -> do
                tmpDir <- getTemporaryDirectory
                let out = tmpDir </> "hpdft-form-export-test.pdf"
                BS.writeFile out bytes
                opened <- openDocument out Nothing
                return $ case opened of
                  Right doc2 ->
                    case pageCount doc2 of
                      Right 1 -> pass "form-export-parent extractFormPdf"
                      Right n ->
                        testFail "form-export-parent extractFormPdf"
                          ("expected 1 page, got " ++ show n)
                      Left err -> testFail "form-export-parent extractFormPdf pageCount" (show err)
                  Left err -> testFail "form-export-parent extractFormPdf reopen" (show err)
          Left err -> return $ testFail "form-export-parent extractFormPdf" (show err)
      Left err -> return $ testFail "form-export-parent open extract" (show err)

runFm42Integration :: Result
runFm42Integration =
  unsafePerformIO $ do
    let path = "/home/k16/work/wwtawwta-systems/publish/8965333188777.pdf"
    exists <- doesFileExist path
    if not exists
      then return $ pass "8965333188777 Fm42 integration (skipped)"
      else do
        result <- openDocument path Nothing
        case result of
          Right doc ->
            case extractFormPdf doc 205 (T.pack "Fm42") of
              Right bytes
                | BSC.take 4 bytes /= BSC.pack "%PDF" ->
                    return $ testFail "8965333188777 Fm42 extract" "missing %PDF header"
                | otherwise -> do
                    tmpDir <- getTemporaryDirectory
                    let out = tmpDir </> "hpdft-fm42-test.pdf"
                    BS.writeFile out bytes
                    opened <- openDocument out Nothing
                    return $ case opened of
                      Right doc2 ->
                        case pageCount doc2 of
                          Right 1 -> pass "8965333188777 Fm42 extract"
                          Right n ->
                            testFail "8965333188777 Fm42 extract"
                              ("expected 1 page, got " ++ show n)
                          Left err -> testFail "8965333188777 Fm42 pageCount" (show err)
                      Left err -> testFail "8965333188777 Fm42 reopen" (show err)
              Left err -> return $ testFail "8965333188777 Fm42 extract" (show err)
          Left err -> return $ testFail "8965333188777 open" (show err)

{-# NOINLINE runFormExportParentNames #-}
{-# NOINLINE runFormExportParentExtract #-}
{-# NOINLINE runFm42Integration #-}

runTaggedFixture :: Result
runTaggedFixture =
  let path = "data/fixtures/tagged.pdf"
  in unsafePerformIO $ do
    result <- pdfToTextTaggedBS path (Just "")
    return $ case result of
      Right bs ->
        let actual = BSLC.unpack bs
            expected = "First paragraph.\n\nSecond paragraph.\n"
        in assertTextEq "tagged.pdf end-to-end" (T.pack expected) (T.pack actual)
      Left err -> testFail "tagged.pdf end-to-end" (show err)

{-# NOINLINE runTaggedFixture #-}

textStreamResults :: [Result]
textStreamResults =
  [ runTextStreamConcat
  , runTextStreamPageOrder
  ]

runTextStreamConcat :: Result
runTextStreamConcat =
  unsafePerformIO $ do
    result <- openDocument "data/fixtures/multipage.pdf" Nothing
    case result of
      Left err -> return $ testFail "pdfToTextStreamDoc concat open" (show err)
      Right doc -> do
        let (expected, _) = pdfToTextDoc doc
        ref <- newIORef BSL.empty
        _ <- pdfToTextStreamDoc doc $ \_ _ bs -> modifyIORef ref (<> bs)
        actual <- readIORef ref
        return $ assertTextEqLazy "pdfToTextStreamDoc concat == pdfToTextDoc" expected actual

{-# NOINLINE runTextStreamConcat #-}

runTextStreamPageOrder :: Result
runTextStreamPageOrder =
  unsafePerformIO $ do
    result <- openDocument "data/fixtures/multipage.pdf" Nothing
    case result of
      Left err -> return $ testFail "pdfToTextStreamDoc page order open" (show err)
      Right doc -> do
        ref <- newIORef ([] :: [(Int, Int)])
        _ <- pdfToTextStreamDoc doc $ \pg total _ -> modifyIORef ref ((pg, total) :)
        events <- readIORef ref
        return $ assertBool "pdfToTextStreamDoc page order"
          (reverse events == [(1, 3), (2, 3), (3, 3)])

{-# NOINLINE runTextStreamPageOrder #-}

assertTextEqLazy :: String -> BSL.ByteString -> BSL.ByteString -> Result
assertTextEqLazy label expected actual =
  if expected == actual
    then pass label
    else testFail label ("length expected " ++ show (BSL.length expected)
                         ++ ", got " ++ show (BSL.length actual))

tuiScrollResults :: [Result]
tuiScrollResults =
  let st = ScrollState {scrollTopLine = 0, scrollTotalLines = 20, scrollTextRows = 5}
   in [ assertBool "tuiScroll initial top" (scrollTopLine (initialScrollState 5) == 0)
      , assertBool "tuiScroll scrollBy down"
          (scrollTopLine (scrollBy 3 st) == 3)
      , assertBool "tuiScroll clamp at end"
          (scrollTopLine (scrollBy 100 st) == 15)
      , assertBool "tuiScroll scrollToTop"
          (scrollTopLine (scrollToTop (scrollBy 10 st)) == 0)
      , assertBool "tuiScroll scrollToEnd"
          (scrollTopLine (scrollToEnd st) == 15)
      , assertBool "tuiScroll half page down"
          (scrollTopLine (scrollHalfPageDown st) == 2)
      , assertBool "tuiScroll visible range"
          (visibleLineRange (scrollBy 4 st) == (4, 9))
      , assertBool "tuiScroll status line"
          (statusLineNumber (scrollBy 4 st) == 5)
      , assertBool "tuiScroll search forward hit"
          (searchForwardFrom (== "b") 0 ["a", "b", "c", "b"] == Just 1)
      , assertBool "tuiScroll search forward from offset"
          (searchForwardFrom (== "b") 2 ["a", "b", "c", "b"] == Just 3)
      , assertBool "tuiScroll search forward miss"
          (searchForwardFrom (== "z") 0 ["a", "b"] == Nothing)
      , assertBool "tuiScroll search forward negative start clamps"
          (searchForwardFrom (== "a") (-3) ["a", "b"] == Just 0)
      , assertBool "tuiScroll search backward hit"
          (searchBackwardFrom (== "b") 3 ["a", "b", "c", "b"] == Just 3)
      , assertBool "tuiScroll search backward from offset"
          (searchBackwardFrom (== "b") 2 ["a", "b", "c", "b"] == Just 1)
      , assertBool "tuiScroll search backward miss"
          (searchBackwardFrom (== "z") 3 ["a", "b"] == Nothing)
      , assertBool "tuiScroll search backward negative start"
          (searchBackwardFrom (== "a") (-1) ["a"] == Nothing)
      , assertBool "tuiScroll display width ascii"
          (stringDisplayWidth "abc" == 3)
      , assertBool "tuiScroll display width CJK"
          (stringDisplayWidth "あい" == 4)
      , assertBool "tuiScroll display width mixed"
          (stringDisplayWidth "/検索x" == 6)
      , assertBool "tuiScroll clip CJK at boundary"
          (clipToDisplayWidth 3 "あい" == "あ")
      , assertBool "tuiScroll clip mixed"
          (clipToDisplayWidth 4 "aあい" == "aあ")
      , assertBool "tuiScroll pad CJK to width"
          (padToDisplayWidth 6 "あ" == "あ    ")
      , assertBool "tuiScroll pad straddling wide char"
          (stringDisplayWidth (padToDisplayWidth 4 "あい") == 4)
      ]

cmapTestFontInfo :: Encoding -> FontInfo
cmapTestFontInfo enc =
  FontInfo
    { fiEncoding = enc
    , fiToUnicode = M.empty
    , fiWidth = const 1000
    , fiWidthV = const 1000
    , fiWMode = 0
    , fiBytesPerCode = 2
    , fiDefaultWidth = 1000
    }

cmapEncodingResults :: [Result]
cmapEncodingResults =
  let ufi = cmapTestFontInfo UnicodeMap
      jfi = cmapTestFontInfo JISmap
      bmp = [0x65, 0xE5, 0x67, 0x2C]
      mixed = [0x65, 0xE5, 0xD8, 0x3D, 0xDE, 0x00, 0x67, 0x2C]
      odd = [0x65, 0xE5, 0x67]
   in [ assertBool "unicodeBytesToCodes BMP"
          (unicodeBytesToCodes bmp == [0x65E5, 0x672C])
      , assertBool "unicodeBytesToCodes surrogate pair"
          (unicodeBytesToCodes [0xD8, 0x3D, 0xDE, 0x00] == [0x1F600])
      , assertBool "unicodeBytesToCodes BMP and surrogate mixed"
          (unicodeBytesToCodes mixed == [0x65E5, 0x1F600, 0x672C])
      , assertBool "unicodeBytesToCodes odd trailing byte"
          (unicodeBytesToCodes odd == [0x65E5])
      , assertBool "bytesToCodes UnicodeMap via FontInfo"
          (bytesToCodes ufi bmp == [0x65E5, 0x672C])
      , assertTextEq "encodingUnicode UnicodeMap BMP"
          "\x65E5"
          (encodingUnicode UnicodeMap 0x65E5)
      , assertTextEq "encodingUnicode UnicodeMap invalid"
          "\xFFFD"
          (encodingUnicode UnicodeMap 0x110000)
      , assertTextEq "encodingUnicode JISmap 0x3021"
          "\x4E9C"
          (encodingUnicode JISmap 0x3021)
      , assertTextEq "encodingUnicode JISmap 0x2422"
          "\x3042"
          (encodingUnicode JISmap 0x2422)
      , assertTextEq "encodingUnicode JISmap ASCII"
          "A"
          (encodingUnicode JISmap 0x41)
      , assertTextEq "encodingUnicode JISmap missing"
          "\xFFFD"
          (encodingUnicode JISmap 0x2021)
      , assertBool "jisx0208Map has 日本語 codes"
          (M.member 0x467C jisx0208Map && M.member 0x4B5C jisx0208Map)
      , assertBool "bytesToCodes JISmap 2-byte fixed"
          (bytesToCodes jfi [0x46, 0x7C, 0x4B, 0x5C] == [0x467C, 0x4B5C])
      ]

normalizePdfNumberResults :: [Result]
normalizePdfNumberResults =
  let cases =
        [ (".5", 0.5)
        , ("-.5", -0.5)
        , ("-.23999999", -0.23999999)
        , ("1.5", 1.5)
        , ("-1.5", -1.5)
        , ("5.", 5.0)
        ]
      abnormal =
        [ ""
        , "-"
        , "+"
        , "-.5."
        , "."
        , "-."
        ]
   in [ assertDoubleEq ("StreamLex.parsePdfNumber " ++ s) n (parsePdfNumber s)
      | (s, n) <- cases
      ]
      ++ [ assertBool ("StreamLex.parsePdfNumber abnormal " ++ show s)
             (parsePdfNumber s `seq` True)
         | s <- abnormal
         ]
      ++ [ assertTextEq ("StreamLex.normalizePdfNumber " ++ s) (T.pack expected) (T.pack (normalizePdfNumber s))
         | (s, expected) <- [(".5", "0.5"), ("-.5", "-0.5"), ("-.23999999", "-0.23999999")]
         ]

encryptSpecResults :: [Result]
encryptSpecResults =
  [ assertBytesEq label expected actual | (label, expected, actual) <- encryptSpecCases ]

heightSpecResults :: [Result]
heightSpecResults =
  [ assertBool "parseHeightSpec rows" (parseHeightSpec "12" == Just (HeightRows 12))
  , assertBool "parseHeightSpec percent" (parseHeightSpec "50%" == Just (HeightPercent 50))
  , assertBool "parseHeightSpec rejects bad percent"
      (parseHeightSpec "150%" == Nothing)
  , assertBool "viewportHeight default half" (viewportHeight 24 Nothing == 12)
  , assertBool "viewportHeight rows clamp min"
      (viewportHeight 24 (Just (HeightRows 3)) == 6)
  , assertBool "viewportHeight rows clamp max"
      (viewportHeight 24 (Just (HeightRows 40)) == 24)
  , assertBool "viewportHeight percent"
      (viewportHeight 20 (Just (HeightPercent 50)) == 10)
  , assertBool "viewportHeight percent full"
      (viewportHeight 10 (Just (HeightPercent 100)) == 10)
  ]

