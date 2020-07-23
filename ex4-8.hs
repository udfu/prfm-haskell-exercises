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
third2 xs | length xs >= 3 = xs !! 2
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
