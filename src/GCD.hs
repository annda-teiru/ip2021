module GCD where

{-

------------------------------ a
-----------------                    b
-}

import Numeric.Natural

gcd' :: Natural -> Natural -> Natural
gcd' a b
    | b == 0 = a
    | otherwise = gcd' b (mod a b)
    