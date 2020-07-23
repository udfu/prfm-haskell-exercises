-- 1
halve :: [a] -> ([a],[a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

-- 2
third1 :: [a] -> a
third2 :: [a] -> a
third3 :: [a] -> a
-- with head & tail
third1 xs | length xs >= 3 = head (tail (tail xs)) 
-- with list indexing !!
third2 xs | length xs >= 3 = xs Prelude.!! 2
-- with pattern matching
third3 (_:_:x:xs) = x

-- 3
safetail1 :: [a] -> [a]
safetail2 :: [a] -> [a]
safetail3 :: [a] -> [a]
-- conditional expression
safetail1 xs = if null xs then [] else tail xs
-- guarded equation
safetail2 xs | null xs = []
             | otherwise = tail xs
-- pattern matching
safetail3 [] = []
safetail3 xs = tail xs

-- 4 show how disjunction (!!) could be defined in 4 similiar ways with pattern matching
-- (!!) :: Bool -> Bool -> Bool
-- True !! True = True
-- True !! False = True
-- False !! True = True
-- False !! False = False

-- 5 
-- hint: use two nested conditional expression

-- Example with pattern matching
-- True && True = True
-- _ && _ = False

-- Solution with conditional expresion
(&&) :: Bool -> Bool -> Bool
-- b1 && b2 = if b1 then 
--                  if b2 then True else False
--            else False

-- 6
-- Example 
-- True && b = b
-- False && _ = False

b1 && b = if b1 then b else False

-- 7 - show how the meaning of this curried function can be formalised using lambda expressions
-- curried function
mult :: Int -> Int -> Int -> Int 
-- mult x y z = x*y*z

mult = \x -> (\y -> (\z -> x * y * z))

-- 8 

luhnDouble :: Int -> Int 
luhnDouble x | x > 4 = 2*x -9
             | otherwise = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d | ((luhnDouble b) + (luhnDouble c) + (luhnDouble d)) `mod` 10 == 0 = True
             | otherwise =  False

