-- 7

-- 1 yes, String 
-- 2 no
-- 3 yes, Eq => a -> a -> Bool
-- 4 yes, a -> (a,a)
-- 5 yes  Ord => a -> a -> Bool

-- 8

-- 1
concatEx1 :: [[a]] -> [a]
---2
concatEx1 [] = []
concatEx1 (xs:xss) = xs ++ concatEx1 xss
--3
concatEx2 :: [[a]] -> [a]
concatEx2 [] = []
concatEx2 xss = [x | xs <- xss, x <- xs]
--4
--concatEx3 = foldr (++) [] 
--5
--[[]]

--9
--1 
minElem :: Ord a => [a] -> a

--2
minElem [x] = x
minElem (x:xs) = min x (minElem xs)

-- 10
--1
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x 
--2
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f = \(x,y) -> f x y

--12
--1
--getLine :: IO String
--2
getLine = do { x <- getChar
               if x == '\n' then 
                   return
               else
                   do { xs <- getLine
                        return (x:xs)}}









--9



