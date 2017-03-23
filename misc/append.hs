append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x:(append xs ys)

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = append (rev xs) [x]

appendRev :: [a] -> [a] -> [a]
appendRev xs ys = append (rev xs) ys
{-appendRev xs ys = append (rev xs) ys
-}{-appendRev [] ys == append (rev []) ys       
appendRev [] ys == append [] ys   -}   

appendRevRedux :: [a] -> [a] -> [a]
appendRevRedux [] ys = ys
{-appendRev (x:xs) ys == append (rev (x:xs)) ys
appendRev (x:xs) ys == rev (x:xs) ++ ys
appendRev (x:xs) ys == append (rev xs) [x] ++ ys
appendRev (x:xs) ys == (rev xs) ++ [x] ++ ys-}
appendRevRedux (x:xs) ys = (rev xs) ++ [x] ++ ys

revRedux :: [a] -> [a]
revRedux []     = []
revRedux (x:xs) = (revRedux xs) ++ [] ++ [x]
