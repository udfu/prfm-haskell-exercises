-- repetition file

-- 5.1
sumOf100Squares :: Int
sumOf100Squares = sum [x^2 | x <- [1..100]]
-- 5.2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]
-- 5.4
replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1..n]]
-- 5.5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
-- 5.6
-- factors :: Int -> [Int]
-- factors n = [x | x <- [1..n], n `mod` x == 0]
-- perfects :: Int -> [Int]
-- perfects n = [x | x <- [1..n], sum (factors (x-1)) == x]


-- Euler Sum / DBS2 exercises
eulerSum :: Int -> Int
eulerSum n = sum [x | x <- [0..n], (x `mod` 3 == 0) || (x `mod` 5 == 0)]

-- fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

fibonacciSeries :: Int -> [Int]
fibonacciSeries n  = fibonacciSeriesCalc 1 n
fibonacciSeriesCalc :: Int -> Int -> [Int]
fibonacciSeriesCalc n m | n < m = fibonacci n : fibonacciSeriesCalc (n+1) m
                        | otherwise = []


-- exam
concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (xs:xss) = xs ++ concat1 xss
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 xss = [x | xs <- xss, x <- xs]


minElement :: Ord a => [a] -> a
minElement [a] = a
minElement (x:xs) = min x (minElement xs)

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

-- testat :-)

-- data Tree n l = Leaf l | Node n (Tree n l) (Tree n l)

-- equals :: (Eq n, Eq l) => Tree n l -> Tree n l -> Bool
-- equals (Leaf a) (Leaf b) = a == b
-- equals (Node a b c) (Node x y z) = a == x && equals b y && equals c z
-- -- equals (Node "PLUS" (Leaf 1) (Leaf 2)) (Node "PLUS" (Leaf 1) (Leaf 2)) 
-- -- equals (Node "PLUS" (Leaf 1) (Leaf 2)) (Node "PLUS" (Leaf 2) (Leaf 2))

-- instance (Eq n, Eq l) => Eq (Tree n l) where
--     (==) = equals

-- showInorder :: (Show a1, Show a2) => Tree a2 a1 -> String
-- showInorder (Leaf x) = show x
-- showInorder (Node x y z) = showInorder y ++ show x ++ showInorder z

-- instance (Show n, Show l) => Show (Tree n l) where
--      show = showInorder

-- eval (Leaf l) = l
-- eval (Node x y z) = x (eval y) (eval z) 
-- --eval (Node (+) (Leaf 1) (Leaf 2))
-- --eval (Node (++) (Leaf "abc") (Leaf "def"))

-- member x (Leaf y) = x == y
-- member x (Node a b c) = a == x || member x b || member x c
-- --member 'a' (Node '+' (Leaf 'a') (Leaf 'b'))

-- exam 12 

getLineEx :: IO String
getLineEx = do {
                x <- getChar; 
                if x == '\n' then
                        return []
                    else 
                        do {
                            xs <- getLineEx;
                            return (x:xs)
                        }
}

-- 8.1
data Nat = Zero | Succ Nat
    deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mul :: Nat -> Nat -> Nat
mul m Zero = Zero
mul m (Succ n) = add m (mul m n)

-- 8.2

-- 8.3
data Tree' a = Leaf a | Node (Tree' a) (Tree' a)

diff :: Int -> Int -> Int 
diff n m | n > m = n - m
         | otherwise = m - n 

leaves :: Tree' a -> Int
leaves (Leaf x) = 1
leaves (Node x y) = leaves x + leaves y

balanced :: Tree' a -> Bool
balanced (Leaf x) = True
balanced (Node x y) | (diff (leaves x) (leaves y)) <= 1 = True
                    | otherwise = False

-- 8.4

halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)           

balance :: [a] -> Tree' a
balance [x] = (Leaf x)
balance xs = Node (balance (fst(halve xs))) (balance (snd(halve xs)))

-- 8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 8.6

eval :: Expr -> Int 
eval (Val n) = n
eval (Add x y) = (eval x) + (eval y) 

evalFolde :: Expr -> Int 
evalFolde = folde id (+)

size :: Expr -> Int 
size = folde (const 1) (+)

-- 7.1
mapfilter f p xs = map f (filter p xs)

-- 7.2 
allEx p = and . map p

anyEx p = or . map p

takeWhileEx p (x:xs) | p x = x : (takeWhileEx p xs)
                     | otherwise = []

dropWhileEx p (x:xs) = if p x then dropWhileEx p xs else (x:xs)

-- 7.3
mapEx f = foldr (\x xs -> f x : xs) []

filterEx p = foldr (\x xs -> if p x then x : xs else xs) []

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y ) 0

-- 7.5
curryEx :: ((a,b) -> c) -> a -> b -> c
curryEx f = (\x y -> f (x,y))

uncurryEx :: (a -> b -> c) -> (a,b) -> c
uncurryEx f = (\(x,y) -> f x y)

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = (f x) : altMap g f xs 

-- 9.4
readInt :: IO Int
readInt = do { n <- getLine; 
            return (read n)
}

adder :: IO()
adder = do { putStr "How many numbers: " ;
             n <- readInt;
             numbers <- reader n [];
             putStrLn ("Sum is " ++ show (sum numbers)) 
}

reader lines ns = do {
                    n <- readInt; 
                    if (lines-1) > 0 then
                        reader (lines - 1) (n:ns)
                    else 
                        return (n:ns)}
