module Grains (square, total) where

square :: Integer -> Maybe Integer
square x
  | x < 1 || x > 64 = Nothing
  | otherwise = Just $ 2 ^ (x - 1)

total :: Integer
total = 2 ^ square - 1
sum $ mapMaybe square [1..64]