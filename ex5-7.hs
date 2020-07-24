-- 1
sumOf100Squares :: Int
sumOf100Squares = sum [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0..m], y <- [x..n]]

-- 3
square :: Int -> [(Int,Int)]
square n = grid n n

-- 4
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]]

-- 5 
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (take ((length (factors x)) - 1) (factors x)) == x]

