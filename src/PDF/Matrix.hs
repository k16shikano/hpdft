module PDF.Matrix
  ( Matrix(..)
  , identity
  , mkMatrix
  , multiply
  , apply
  , applyVec
  , translate
  , scale
  , components
  ) where

data Matrix = Matrix !Double !Double !Double !Double !Double !Double
  deriving (Eq, Show)

identity :: Matrix
identity = mkMatrix 1 0 0 1 0 0

mkMatrix :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
mkMatrix = Matrix

multiply :: Matrix -> Matrix -> Matrix
multiply (Matrix a1 b1 c1 d1 e1 f1) (Matrix a2 b2 c2 d2 e2 f2) =
  Matrix
    (a1 * a2 + b1 * c2)
    (a1 * b2 + b1 * d2)
    (c1 * a2 + d1 * c2)
    (c1 * b2 + d1 * d2)
    (e1 * a2 + f1 * c2 + e2)
    (e1 * b2 + f1 * d2 + f2)

apply :: Matrix -> (Double, Double) -> (Double, Double)
apply (Matrix a b c d e f) (x, y) = (a * x + c * y + e, b * x + d * y + f)

applyVec :: Matrix -> (Double, Double) -> (Double, Double)
applyVec (Matrix a b c d _ _) (x, y) = (a * x + c * y, b * x + d * y)

translate :: Double -> Double -> Matrix
translate tx ty = mkMatrix 1 0 0 1 tx ty

scale :: Double -> Double -> Matrix
scale sx sy = mkMatrix sx 0 0 sy 0 0

components :: Matrix -> (Double, Double, Double, Double, Double, Double)
components (Matrix a b c d e f) = (a, b, c, d, e, f)
