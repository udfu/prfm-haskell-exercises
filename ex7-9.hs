-- 1
-- [ f x | x <- xs, p x] --> re-expressed with higher-order functions map and filter
reexpressionEx1 f p xs = map f (filter p xs)

-- 2
-- a
-- allEx :: (a ->  Bool) -> [Bool] -> Bool
allEx p = and . map p
-- b
-- any :: (a -> Bool) -> [Bool] -> Bool
anyEx p = or . map p
-- c
-- takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhileEx p (x:xs) | p x = x : takeWhileEx p xs
                     | otherwise = []
-- d
-- dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhileEx p (x:xs) | p x = dropWhileEx p xs
                     | otherwise = (x:xs)

-- 3
-- map f xs = [f x | x <- xs]
-- mapEx f (x:xs) = foldr f x xs

-- filter p xs = [x | x <- xs , p x]
--filterEx p xs = []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y ) 0

-- 5
-- curry :: 

-- uncurry ::

-- 6 
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8) 

mapUnfold f = unfold null f f 

--iterateUnfold f x = unfold null (f x)  (f x) 


-- 9
-- altMap :: (a -> b) -> (a -> b) -> [a] -> [b]

