module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Int] -> Int -> Int
sumOfMultiples x y = sum $ filter (`findMultiple` x) [1..pred y]
  where
    findMultiple z = any ((==) 0 . mod z)

{-mad version

import Data.List
iteration :: [Int] -> Int -> [Int]
iteration [] y = [0]
iteration (x:xs) y = (tail $ nub $ (takeWhile (>0) $ iterate (subtract x) y) ++ (iteration xs y))

recursiveSum x y = sum (iteration (x) y)-} 