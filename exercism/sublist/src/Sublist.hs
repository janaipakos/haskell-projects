module Sublist (Sublist(..), sublist) where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

checkList :: Eq a => [a] -> [a] -> Bool
checkList xs ys =
  let matchFromStart = any (`elem` xs) ys
      matchLater = xs `checkList` tail ys
  in length xs <= length ys && (matchFromStart || matchLater)


sublist :: Eq a => [a] -> [a] -> Sublist
sublist xs ys
  | length xs < length ys && checkList xs ys = Sublist
  | length xs > length ys && checkList ys xs = Superlist
  | length xs == length ys && xs == ys = Equal
  | otherwise = Unequal

