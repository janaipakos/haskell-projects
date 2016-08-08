-- Mastermind -----------------------------------------
-- A game of Mastermind from Week 2 of CIS 194 2013
-- Call the solve function with a list of peg colors and see how many moves are needed to win

{-# OPTIONS_GHC -Wall #-}

module HW02 where
import Data.List

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
match :: Peg -> Peg -> Int
match x y
    | (x==y) = 1
    | otherwise = 0

exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches (_:_) [] = 0
exactMatches [] (_:_) = 0
exactMatches (x:xs) (y:ys) = (match x y) + (exactMatches xs ys)

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors xs = map pred (map length (group $ sort $ xs ++ colors))

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = length $ xs `intersect` ys 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys = Move ys (exactMatches xs ys) $ (matches xs ys)-(exactMatches xs ys)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move xs x y) ys =  (Move xs x y) == getMove ys xs

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes (Move xs x y) = filter (isConsistent (Move xs x y))

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
    | n <= 0 = []
    | n == 1 = map (:[]) colors
    | otherwise = concatMap f (allCodes (n-1))
            where f code = map (\x -> code ++ [x]) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve n = f (allCodes ( length n))
  where f [] = []
        f (x:xs) = let m = getMove n x
                    in m : f (filterCodes m xs)