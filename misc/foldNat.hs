data Nat = Zero | Add1 Nat deriving (Eq, Ord)

-- How to print natural numbers a nice way :P
instance Show Nat where
  show Zero     = show (0 :: Integer)
  show (Add1 n) = show $ 1 + (read $ show n)

one :: Nat
one = (Add1 Zero)

two :: Nat
two = plus (Add1 Zero) (Add1 Zero)

three :: Nat
three = Add1 (Add1 (Add1 Zero))

five :: Nat
five = Add1 (Add1 (Add1 (Add1 (Add1 Zero))))

-- add two natural numbers together.
plus :: Nat -> Nat -> Nat
plus Zero     y = y
plus (Add1 x) y = Add1 (plus x y)
--plus x x:xs = x + plus (x + xs)
--plus (Add1 Zero) (Add1 Zero) = Add1 (Zero `plus` Zero)
--plus (Add1 Zero) (Add1 Zero) = Add1 (plus Zero Zero)
--plus (Add1 Zero) (Add1 Zero) = Add1 (Zero)

-- minus two natural numbers together.
minus :: Nat -> Nat -> Nat
minus x Zero = x
minus Zero x = Zero
minus (Add1 x) (Add1 y) = minus x y
--plus (Add1 Zero) (Add1 Zero) = Add1 (Zero `plus` Zero)
--plus (Add1 Zero) (Add1 Zero) = Add1 (plus Zero Zero)
--plus (Add1 Zero) (Add1 Zero) = Add1 (Zero)

-- multiply two natural numbers.
times :: Nat -> Nat -> Nat
times Zero     _ = Zero
times (Add1 x) y = plus (times x y) y
--times x x:xs = plus (times x xs) x
--times (Add1 (Add1 Zero)) (Add1 Zero) = plus (times (Add1 Zero) (Add1 Zero)) (Add1 Zero)
--plus (plus (times Zero (Add1 Zero)) (Add1 Zero)) (Add1 Zero)
--plus (plus (Zero)(Add1 Zero)) (Add1 Zero)
--plus (Add1 Zero) (Add1 Zero)
--Add1 (plus Zero Add1 Zero)
--Add1 (Add1 Zero)

-- pow raises its first argument to the power of the second argument.
pow :: Nat -> Nat -> Nat
pow _ Zero     = Add1 Zero
pow x (Add1 y) = (x `pow` y) `times` x

foldNat :: a -> (a -> a) -> Nat -> a
foldNat base recur Zero     = base
foldNat base recur (Add1 n) = recur $ foldNat base recur n
--foldNat base recur (Add1 Zero) = recur $ foldNat base recur Zero
--                               = recur $ base


-- 1. Define `plusFold` that behaves like `plus` but uses `foldNat`.
plusFold :: Nat -> Nat -> Nat
plusFold m n = foldNat n Add1 m

-- 2. Define `timesFold` that behaves like `times` but uses `foldNat`.
timesFold :: Nat -> Nat -> Nat
timesFold m = foldNat Zero (plus m)
--timesFold m = foldNat Zero (plus Add1 (Add1 Zero)) Add1 (Add1 Zero)
--              

-- 3. Define `powFold` that behaves like `pow` but uses `foldNat`.
powFold :: Nat -> Nat -> Nat
powFold m n = foldNat (Add1 Zero) (times n) m

-- BONUS!! This is rather difficult...
fact :: Nat -> Nat
fact Zero     = Add1 Zero
fact (Add1 n) = (Add1 n) `times` (fact n)

factFold :: Nat -> Nat
--factFold   foldNat base,  what to add to base,   how many times
factFold n = foldNat (n) (times (fact (minus n one))) (minus n one)