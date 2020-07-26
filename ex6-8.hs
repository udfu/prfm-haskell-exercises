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

-- 5
{-
length [1,2,3]
= { applying length }
1 + length [2,3]
= { applying length }
1 + 1 + length [3]
= { applying length }
1 + 1 + 1 length []
= { applying length }
1 + 1 + 1 + 0
= { applying + }
3

drop 3 [1,2,3,4,5]
= { applying drop }
drop 2 [2,3,4,5]
= { applying drop }
drop 1 [3,4,5]
= { applying drop }
drop 0 [4,5]
= { applying drop }
[4,5]

init [1,2,3]
= { applying init }
1 : init [2,3]
= { applying init }
1 : (2 : init [3])
= { applying init }
1 : (2: ([]))
= list notation
[1,2]
-}

-- 6
and :: [Bool] -> Bool
and [] = True
and [b] = b
and (b:bs) | b = Main.and bs
         | otherwise = False

-- concatEx [[]] = []
-- concatEx [x:xs] = x ++ concatEx xs

replicateEx :: Int -> a -> [a]
replicateEx 0 x = []
replicateEx n x = x : replicateEx (n-1) x

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs Main.!! (n-1)

elem :: Eq a  => a -> [a] -> Bool
elem _ [] = False
elem y (x:xs) | y == x = True
              | otherwise = Main.elem y xs

-- 7
-- not use insert or isort!
-- insert :: Ord a => a -> [a] -> [a]
-- insert x []     = [x]
-- insert x (y:ys) | x <= y = x : y :ys
--                 | otherwise = y : insert x ys

-- isort :: Ord a => [a] -> [a]
-- isort [] = []
-- isort (x:xs) = insert x (isort xs)
-- not working
merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : y : merge xs ys
                    | otherwise = y : x : merge xs ys
                    

-- 8
-- not working
halve :: [a] -> ([a],[a])
halve xs = (take (length xs `div` 2) xs , drop (length xs `div` 2) xs)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (take (length xs `div` 2) xs)) (msort (drop (length xs `div` 2) xs))

