module Main where

import qualified Data.HashSet          as S
import qualified Data.HashMap.Strict   as H
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import Debug.Trace

import Data.Maybe (fromJust)


range = [-10..10]

diff :: Int -> [Int] -> S.HashSet Int
diff x r = let
    ys = S.fromList $ map (subtract x) r
    in S.delete x ys

main = do
    content <- map (fst. fromJust . BC.readInt) . BC.lines <$> B.readFile "2sum.txt"
    let values = foldr S.insert S.empty content
    let table = H.mapWithKey (\x _ -> S.size (diff x range `S.intersection` values) > 0) $ S.toMap values
    let result = H.filter id table
    print $ H.size result