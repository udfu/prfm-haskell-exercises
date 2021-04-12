-- 1
data Nat = Zero | Succ Nat
    deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (Succ n) = add m (mul m n)
{-
3 * 2
mult (Succ (Succ (Succ Zero))) (Succ (Succ (Succ Zero)))

-}

-- 2


-- 3
data Tree a = Leaf a | Node (Tree a) (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9))

numberOfLeafs :: Tree a -> Int
numberOfLeafs (Leaf x)   = 1
numberOfLeafs (Node x y) = numberOfLeafs x + numberOfLeafs y

diff :: Int -> Int -> Int
diff n m | n > m     = n - m
         | otherwise = m - n  

balanced :: Tree a -> Bool
balanced (Leaf x) = True
balanced (Node x y) | diff (numberOfLeafs x) (numberOfLeafs y) <= 1 = balanced x && balanced y
                    | otherwise = False
-- Test: balanced (Node (Node (Leaf 1) (Leaf 4)) (Node (Leaf 6) (Leaf 9)))

--4
halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)

-- balance :: [a] -> Tree
-- balance [x] = Leaf x
-- balance xs = Node (balance (fst (halve xs))) (balance (snd (halve xs)))

--5
data Expr = Val Int | Add Expr Expr

-- folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
-- wtf???



