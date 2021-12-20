{-# LANGUAGE ScopedTypeVariables #-}
module Sort where

import Data.List hiding (insert)

sample :: [Int]
sample = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3]

{-
選択ソート(selection sort)
挿入ソート(insertion sort)
-}

isort :: [Int] -> [Int]
isort xs = iter [] xs
    where
        iter :: [Int] -> [Int] -> [Int]
        iter ys []     = ys
        iter ys (z:zs) = iter (insert z ys) zs

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x < y = x : (y : ys)
    | otherwise = y : (insert x ys)

ssort :: [Int]-> [Int]
ssort [] = []
ssort xs = case select xs of
    (y,ys) -> y : ssort ys

select :: [Int] -> (Int, [Int])
select xs = (x', xs')
    where
        x' = minimum xs
        xs' = delete x' xs