{-# LANGUAGE OverloadedStrings #-}
import PDF.Matrix (Matrix, identity, multiply, apply, applyVec, translate, scale, mkMatrix, components)
import PDF.Definition (Obj(..), FontInfo(..), Encoding(..))
import PDF.DocumentStructure (parseCIDWidths, simpleWidthAt)
import PDF.Interpret
  ( Glyph(..)
  , Rect(..)
  , PageItem(..)
  , interpretContentWithFonts
  , interpretContentWithFontsItems
  )
import PDF.Layout (LayoutOptions(..), defaultLayoutOptions, needsAozoraBar, aozoraRuby, layoutParagraphs, layoutParagraphsWith, layoutPageText, layoutDocument, sortLinesByReadingOrder, linesFromGlyphs, Line(..))
import PDF.Structure (StructElem(..), StructKid(..), structTree, logicalOrder)
import PDF.Document (Document(..))
import PDF.Text (pdfToTextTaggedBS)
import PDF.Error (PdfResult)

import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T

import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (when)

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
          ++ taggedEndToEndResults
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
  Document (M.fromList [("/Root", ObjRef 1)]) objs Nothing

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
