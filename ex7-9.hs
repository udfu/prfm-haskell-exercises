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

-- d
-- dropWhile :: (a -> Bool) -> [a] -> [a]