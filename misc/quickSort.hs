quickSort :: Ord a => [a] -> [a]
--base case
quickSort []     = []
--recursviely call quickSort on left (<) and right (>) then combine results
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
    where left   = [y | y <- xs, y <= x]
          right  = [y | y <- xs, y > x]