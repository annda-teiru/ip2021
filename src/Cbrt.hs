module Cbrt where

import Prelude hiding (sqrt)

cube :: Double -> Double
cube x = x * x * x

square :: Double -> Double
square x = x * x

{-
sqrt :: Double -> Double
もし、sqrt x = y だとしたら、 x = y * y
-}

sqrt :: Double -> Double
sqrt x =  (sqrtIter 1.0) x

sqrtIter :: Double -> (Double -> Double)
sqrtIter guess x
    | ok guess x = guess
    | otherwise  = sqrtIter (improve guess x) x

ok :: Double -> (Double -> Bool)
ok guess x = abs (square guess - x) < 0.0001

improve :: Double -> (Double -> Double)
improve guess x = (guess + x / guess) / 2
