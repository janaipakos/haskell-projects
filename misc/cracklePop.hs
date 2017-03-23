cracklePop :: Int -> String
cracklePop x
  | x `mod` 15 == 0 = "CracklePop"
  | x `mod` 5   == 0 = "Pop"
  | x `mod` 3   == 0 = "Crackle"
  | otherwise            = show x

main :: IO ()
main = mapM_ (putStrLn . cracklePop) [1..100]
