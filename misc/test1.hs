functionC x y =
    case (x > y)  of
        True -> x
        False -> y

ifEvenAdd2 n =
    case (n `mod` 2 == 0) of
        True -> (n+2)
        False -> n

nums x =
    case compare x 0 of
        LT -> -1 
        GT -> 1

dodgy :: Int -> Int -> Int
dodgy x y = x + y * 10 
oneIsOne ::  Int -> Int
oneIsOne = dodgy 1 
oneIsTwo :: Int -> Int
oneIsTwo = (flip dodgy) 2

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum n
    | n == 1         = 1
    | otherwise = n + recursiveSum (n - 1)

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b<a     = Node (insert' b left) a right 
    | b>a     = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left)  (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf) 

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected 
                      then print "yup okay!"
                      else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder = undefined

inorder :: BinaryTree a -> [a]
inorder = undefined

postorder :: BinaryTree a -> [a]
postorder = undefined

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO () 
testPreorder =
  if preorder testTree == [2, 1, 3] 
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO () 
testInorder =
  if inorder testTree == [1, 2, 3] 
  then putStrLn "Inorder fine!" 
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO () 
main = do
  testPreorder
  testInorder
  testPostorder