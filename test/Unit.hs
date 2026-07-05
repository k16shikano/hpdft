import PDF.Matrix
import PDF.Definition (Obj(..))
import PDF.DocumentStructure (parseCIDWidths, simpleWidthAt)

import qualified Data.Map as M

import System.Exit (exitFailure)
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
          ++ parseCIDWidthsResults
          ++ simpleWidthAtResults
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
