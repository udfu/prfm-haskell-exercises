-- 1
{-
[ Char]
(Char, Char, Char)
[(Bool, Char)]
([Bool], [Char])
[[a] -> [a]]
-}

-- 2
bools :: [Bool]
bools = [ True, False, True]

nums :: [[Int]]
nums = [[1,2], [3,4]]

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply a b = a b

-- 3
second :: [a] -> a
second xs = head (tail xs)  

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)  

pair :: a -> b -> (a,b)
pair x y = (x,y) 

double :: Int -> Int
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)