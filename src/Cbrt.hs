{-# LANGUAGE ScopedTypeVariables #-}
module Cbrt where

-- import Prelude hiding (sqrt)

cube :: Double -> Double
cube x = x * x * x

square :: Double -> Double
square x = x * x

{-
sqrt :: Double -> Double
もし、sqrt x = y だとしたら、 x = y * y
-}

sqrt' :: Double -> Double
sqrt' x =  (sqrtIter 1.0) x

sqrtIter :: Double -> (Double -> Double)
sqrtIter guess x
    | ok guess x = guess
    | otherwise  = sqrtIter (improve guess x) x
    where
        ok :: Double -> (Double -> Bool)
        ok guess x = abs (square guess - x) < 0.0001
        improve :: Double -> (Double -> Double)
        improve guess x = (guess + x / guess) / 2
{-
たとえば √２ = aを考えると、
a * a = 2
推測値を g とする
本来は
g * g = 2 になってほしい
g * h = 2 となるように h のを考える
g = 1.0 にしたら　h = 2.0 そこで改良した推測値 g' = (g + h) / 2　とする
g' = 1.5 h' = 2 / g' = 1.333   g'' <- (g' + h') / 2
-}

cbrt :: Double -> Double
cbrt x = x ** (1 / 3)

cbrt' :: Double -> Double
cbrt' x = cbrtIter 1.0 x

cbrtIter :: Double -> Double -> Double
cbrtIter guess x
    | ok guess x = guess
    | otherwise = cbrtIter (improve guess x) x
    where
         ok :: Double -> Double -> Bool
         ok guess x  = abs (cube guess - x) < 0.0001
         improve :: Double -> Double -> Double
         improve guess x = (2 * guess + x / square guess) / 3

{-
たとえば、cbrt 2 = a
a * a * a = 2
g * g * g = 2 になってほしい
g * g * h = 2 ような h を考える
g = 1.0 だったら、h = 2 / square 1.0 = 2.0
g' = (2 * g + h) / 3 = 1.333, h'
-}
