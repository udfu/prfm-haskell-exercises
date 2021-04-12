-- define function in haskell

sum1 [] = 0
sum1 (n:ns) = n + sum1 ns

-- sum [1,2,3,4,5]
-- sum [1..5]

-- math     haskell
-- f(x)     f x
-- f(x,y)   f x y
-- f(g(x))  f (g x)
-- f(x)g(y) f x * f y

-- some haskell commands
-- head, tail, product, reverse, length, take, drop

-- basic types
-- Bool, Char, String, Integer, Float, Double

-- list, tuples types
-- [False,True] :: [Bool], ["test1", "test2"] :: [String], (False,True) :: (Bool,Bool)

-- function type
-- not :: Bool -> Bool
-- even :: Int -> Bool

-- polymorphic types e.g. length :: [a] -> Int

-- overloaded types

-- builtin basic-classes
-- Eq - equality types, Ord - ordered types, Show & Read types, Numeric, Integral, Fractional  

-- conditional expressions
signum1 :: Int -> Int
signum1 n = if n < 0 then -1 else
                if n == 0 then 0 else 1

-- guarded expressions
signum2 :: Int -> Int 
signum2 n | n < 0       = -1
          | n == 0      = 0
          | otherwise   = 1

-- pattern matching
not :: Bool -> Bool
not False = True
not True = False

-- list pattern matching
head1 :: [a] -> a
head1 (x:_) = x

-- lambda expressions (avoid having to name functions that is only referenced once in a program)
odds :: Int -> [Int]
-- example
-- odds n = map f [0..n-1]
--          where f x = x*2+1
odds n = map (\x -> x*2 + 1) [0..n-1]


luhnDouble :: Int -> Int 
luhnDouble x | x > 4 = 2*x -9
             | otherwise = 2*x


altMap f g [] = []
altMap f g (x:xs) = (f x) : altMap g f xs

luhn :: [Int] -> Bool
luhn ns | sum (altMap (+0) luhnDouble (reverse ns)) `mod` 10 == 0 = True
        | otherwise = False 


-- list comprehensions
factors :: Int -> [Int] 
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ] -- guard

-- recursion
fac :: Int -> Int 
fac 0 = 1
fac n = n * fac (n-1)

ownReverse :: [a] -> [a]
ownReverse [] = []
ownReverse (x:xs) = reverse xs ++ [x]

------------------------------------------

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                       smaller = [a | a <- xs, a <= x]
                       larger  = [b | b <- xs, b > x] 

-- qsort [66,33,22,100,2000,1,457,8,6,33,1112,1245]
-- qsort ['e','j','f','h','b','k','a','i','g','d']

-- higher-order functions

-- currying - arguments taken one at a time 
-- functions can return functions

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- map even [1,2,3,4,5,6]
-- map (+1) [1,2,3,4,5,6]
-- type 
mapList f xs = [f x | x <- xs] -- simpler

mapRec f [] = []
mapRec f (x:xs) = f x : map f xs -- preferable for reasoning purposes

-- foldr f v  (encapsulates pattern of defining functions on lists)
ownSum :: Num a => [a] -> a
ownSum = foldr (+) 0 -- implicit argument xs - partial application! maps the empty list to the value v

-- composition operator . returns the composition of two functions as a single function
twice2 f = f . f


-- further topics and advances features
-- declaring own data types and classes
-- interactive programming - yes also possible in haskell :-)
-- functors, applicatives and monads
-- monadic parsing
-- foldables
-- lazy evaluation
-- reasoning about programs
-- calculating compilers


