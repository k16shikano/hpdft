import PDF.Matrix

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
