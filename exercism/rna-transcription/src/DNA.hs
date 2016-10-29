module DNA (toRNA) where

import Data.Traversable

toRNA :: String -> Maybe String
toRNA = traverse fromChar

fromChar :: Char -> Maybe Char
fromChar x = case x of
    'G' -> Just 'C'
    'C' -> Just 'G'
    'T' -> Just 'A'
    'A' -> Just 'U'
    x   -> Nothing