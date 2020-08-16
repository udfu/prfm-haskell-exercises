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
