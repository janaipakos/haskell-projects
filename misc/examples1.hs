-- CracklePop --

import Data.List
main = mapM_ (putStrLn . cracklePop) [1..100]

crackle :: Int -> String
crackle x = if x `mod` 3 == 0 then "Crackle" else ""

pop :: Int -> String
pop x = if x `mod` 5 == 0 then "Pop" else ""

cracklePop :: Int -> String
cracklePop x = if crackle(x) ++ pop(x) == "" 
  then show x 
  else crackle(x) ++ pop(x)

counter x = (\x -> 
                         (\y -> 
                           (\z -> z + x + y) 1) 1) 1



ifEven = (\x -> x * 3) 6


names = [("Ian", "Curtis"), ("Bernard","Sumner"),
                  ("Peter", "Hook"), ("Stephen","Morris")]




myFilter test [] = []
myFilter test (x:xs) = if test x
                                    then myFilter test xs
                                    else x:myFilter test xs

myProduct xs = foldl (*) 1 xs

sumOfSquares xs = foldl (+) 0 $ map (^2) xs