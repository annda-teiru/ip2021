module Sorts where

isSorted :: Ord a => [a] -> Bool
isSorted []       = True
isSorted (x : []) = True
isSorted (x : (y : zs)) -- 要素 : 後続リスト
        = (x <= y) && (isSorted (y : zs))

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "minimum: empty"
myMinimum (x : []) = x
myMinimum (x : (y : zs))
    | x <= y      = myMinimum (x : zs)
    | otherwise = myMinimum (y : zs)

sample1 :: [Int]
sample1 = [3,1,4,1,5,9,2,6,5,3,5,9]

-- selection sort 選択ソート

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = case select xs of
    (y, ys) -> y : ssort ys

select :: Ord a => [a] -> (a, [a])
select [] = error "select: empty"
select (x : []) = (x, [])
select (x : xs) = case select xs of
    (y, ys) | x <= y          -> (x, xs)
            | otherwise       -> (y, x:ys)

isort :: Ord a => [a] -> [a]
isort []          = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x []        = [x]
insert x (y:ys)
    | x <= y      = x : (y : ys)
    | otherwise = y : insert x ys

-- quick sort クイックソート
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort ls ++ [x] ++ qsort rs
    where
        ls = [ y | y <- xs, y <  x ]  -- filter (< x)  xs
        rs = [ z | z <- xs, x <= z ]  -- filter (x <=) xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p []     = ([], [])
partition p (x:xs) = case partition p xs of
    (ys, zs) -> if p x then (x:ys, zs) else (ys, x:zs)

qsort2 :: Ord a => [a] -> [a]
qsort2 [] = []
qsort2 (x:xs) = qsort2 ls ++ [x] ++ qsort2 rs
    where
        (ls, rs) = partition (< x) xs

bigdata :: [Int]
bigdata = cycle sample1

bigdata3 :: [Int]
bigdata3 = take 1000 bigdata
bigdata4 :: [Int]
bigdata4 = take 10000 bigdata
bigdata5 :: [Int]
bigdata5 = take 100000 bigdata
bigdata6 :: [Int]
bigdata6 = take 1000000 bigdata
bigdata7 = take 10000000 bigdata

{-
3 → 1 → 4 → 1 → ... → 9 → []

xyxle xs = ys
    where
        ys = xs ++ ys
-}
msort :: Ord a => [a] -> [a]
msort = mergeAll . pairs

pairs :: Ord a => [a] -> [[a]]
pairs []       = []
pairs (x:[])   = [[x]]
pairs (x:y:zs)
    | x > y     = [y,x] : pairs zs
    | otherwise = [x,y] : pairs zs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x > y     = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

mergeAll :: Ord a => [[a]] -> [a]
mergeAll [] = []
mergeAll [xs] = xs
mergeAll xss = case divid xss of
    (yss, zss) -> merge (mergeAll yss) (mergeAll zss)

divid :: [a] -> ([a],[a])
divid [] = ([],[])
divid [x] = ([x], [])
divid (x:y:zs) = case divid zs of
    (ps,qs) -> (x:ps, y:qs)

{-
計算の手間(時間計算量)

サイズnの産家のリスト
(1) v
    [3,1,4,1,5,9]
        |
        v
    [3,1,4,1,5,9]
         |
        v
    [3,1,4,1,5,9]
            |
        v
    [3,1,4,1,5,9]
             |
        v
    [3,1,4,1,5,9]
                    |

    最少は1
    (n-1)回の比較で最小値がまとまります。

    (n-1) + (n-2) + ... + 2     + 1     = S
    1     +  2    + ... + (n-2) + (n-1) = S

      n   +  n    + ... + n     + n     = 2S
     n (n - 1) = 2S

     n^2 - n = 2S
     
     (n^2 - n)
     --------- = S
         2

         n^2    n
     S = --- - ---
         2      2

     0 (n^2)
-}

{-
                  16
            8           8
         4     4      4    4
       2  2   2  2  2  2  2  2
         4     4      4    4
            8           8
                  16

比較の回数は1段ごとに0(n)である。
段数は0(log n)対数の底は2

全体 0(n log n)
-}