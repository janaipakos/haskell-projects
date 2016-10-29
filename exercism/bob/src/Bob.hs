module Bob (responseFor) where

import Data.List

responseFor :: String -> String
responseFor x
  | length onlyWords == 0 = "Fine. Be that way!"
  | last onlyWords == '?' &&  responseLength /= True= "Sure."
  | responseLength && 'ä' `notElem` onlyWords = "Whoa, chill out!"
  | otherwise = "Whatever."
    where 
      onlyUpper = (['A'..'Z'] `intersect` x)
      onlyLower = (['a'..'z'] `intersect` x)
      responseLength = length onlyUpper > length onlyLower
      onlyWords = unwords (words x)