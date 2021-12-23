{-# LANGUAGE ScopedTypeVariables #-}
module Nat where

import Prelude hiding ((+), (*), (^))
import Numeric.Natural

{-
自然数: Natural <-> Integer 整数
          Word <-> Int     64bit
 0 ~ 2^64 - 1 <-> - 2^63 ~ 2^63 - 1
-}

{-
(1)   0 は自然数です
(2)   n が自然数のとき、
      succ n も自然数
(3)   (1)と(2)でつくったものだけが自然数
-}

(+) :: Natural -> Natural -> Natural
-- 0 + n = n                              -- (1)
-- m + n = succ (pred m + n)   -- (2)
{- 
   3 + 4
=> { (2) }
   succ (2 + 4)
=> { (2) }
   succ (succ (1 + 4))
=> { (2) }
   succ (succ (succ (0 + 4)))
=> { (1) }
   succ (succ (succ 4))
=>
   succ (succ 5)
=>
   succ 6
=>
   7
-}
0 + n = n                          -- (1)
m + n = pred m + succ n -- (2)
{-
  3 + 4
= { (2) }
  2 + 5
= { (2) }
  1 + 6
= { (2) }
  0 + 7
= { (1) }
  7
-}

(*) :: Natural -> Natural -> Natural
-- 0 * n = 0                         -- (1)
-- m * n = pred m * n + n  -- (2)
{-
   3 * 4
=> (2)
   (2 * 4) + 4
=> (2)
   ((1 * 4) + 4) + 4
=> (2)
   (((0 * 4) + 4) + 4) + 4
=> (1)
   ((0 + 4) + 4) + 4
=> 
   (4 + 4) + 4
=>
   8 + 4
=>
   12
-}

-- 線形反復プロセスになる定義を書け

m * n = iter 0 m n
  where
    iter :: Natural -> Natural -> Natural -> Natural
    iter k 0 n = k                                    -- (1) 基底ケース
    iter k m n = iter (k + n) (pred m) n  -- (2) 帰納ケース

{-
   iter 0 3 4
=> (2)
   iter 4 2 4
=> (2)
   iter 8 1 4
=> (2)
   iter 12 8 4
=> (1)
   12
-}

-- べき乗
-- b ^ n
-- b ^ 0 = 1
-- b ^ n = b ^ (n-1) * b

-- infixr 8 +
-- infixl 7 *
-- infixl 8 ^

-- (^) :: Natural -> Natural -> Natural
-- b ^ 0 = 1                            -- (1)
-- b ^ n = (b ^ (pred n)) * b  -- (2)

{-
   3 ^ 4
=> (2)
   (3 ^ 3) * 3
=> (2)
   ((3 ^ 2) * 3) * 3
=> (2)
   (((3 ^ 1) * 3) * 3) * 3
=> (2)
=> ((((3 ^ 0) * 3) * 3) * 3) * 3
=> (1)
   (((1 * 3) * 3) * 3) * 3
...
   81
-}

--b ^ n = iter 1 b n
--  where
--       iter :: Natural -> Natural -> Natural -> Natural
--       iter k b 0 = k
--       iter k b n = iter (k * b) b (pred n)

{- 不変条件 k * (b ^ n) は常に一定
   iter 1 4 3
   iter 4 4 2
   iter 16 4 1
   iter 64 4 0
   64

   b ^ 8 = (((((((1 * b) *b) * b) * b) * b) * b) * b) * b
   ですが。。。
   b ^ 2 = b * b
   b ^ 4 = (b ^ 2) * (b ^ 2)
   b ^ 8 = (b ^ 4) * (b ^ 4)

   b^2n        = (b^n)^2
   b^(2n+1) = b * b^2n
-}
-- 2^n = exp 2 n
--expo :: Natural -> Natural -> Natural
--expo b n
--    | n == 0       = 1
--    | even n       = square (expo b (n 'div' 2))
--    | otherwise  = b * expo b (pred n)

--square :: Natural -> Natural
--square n = n * n
