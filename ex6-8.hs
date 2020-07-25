-- 1
factorial :: Int -> Int 
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)
            | otherwise = error "Must be a positive number"

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3 
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ 1 = m
m ^ n = m * (m Main.^ (n-1))

-- 4
euclid :: Int -> Int -> Int 
euclid m n | m == n = m
           | m < n = euclid (n-m) m
           | m > n = euclid (m-n) n

