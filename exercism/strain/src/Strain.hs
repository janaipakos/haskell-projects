module Strain (keep, discard) where

import Data.List

discard :: Eq a => (a -> Bool) -> [a] -> [a]
discard f [] = []
discard f (x:xs) = snd (separateCollection f (x:xs))

keep :: Eq a => (a -> Bool) -> [a] -> [a]
keep f [] = []
keep f (x:xs) = fst (separateCollection f (x:xs))

separateCollection :: (a -> Bool) -> [a] -> ([a], [a])
separateCollection f (x:xs) = partition f (x:xs)