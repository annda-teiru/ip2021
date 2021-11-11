module Factorial where

{-
factorial 階乗階乗
n! = n × (n-1) × ... × 2 × 1
   = n × ((n-1 × ... × 2 × 1))
    = n × (n-1)!
-}
fact :: Integer -> Integer
fact 1 = 1              -- (1) 基底ケース
fact n = n * fact (n-1) -- (2)帰納ケース

{- 線形再帰的計算プロセス
fact 6
6 * fact 5
6 * (5 * fact 4)
6 * (5 * (4 * fact 3))
6 * (5 * (4 * (3 * fact 2)))
6 * (5 * (4 * (3 * (2 * fact 1))))
6 * (5 * (4 * (3 * (2 *1))))
6 * (5 * (4 * (3 * 2)))
6 * (5 * (4 * 6))
6 * (5 * 24)
6 * 120
720
-}

fact' :: Integer -> Integer
fact' n = factIter n 1 1 

factIter :: Integer -> Integer -> Integer -> Integer
factIter maxCount prod count
    | maxCount < count = prod
    | otherwise         = factIter maxCount (prod * count) (count + 1)

{- 反復的計算プロセス
積          ← 積 × カウンタ
カウンター ← カウンター + 1

fact' 6
factIter 6     1 1
factIter 6     1 2
factIter 6     2 3
factIter 6     6 4
factIter 6   24 5
factIter 6 120 6
factIter 6 720 7
720
-}
