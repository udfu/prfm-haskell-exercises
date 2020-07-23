-- 2
-- 2^3*4 -> (2^3) * 4 = 32
-- 2*3+4*5 -> (2*3) + (4*5) = 26
-- 2+3*4^5 -> 2 + (3*(4^5)) = 3074

-- 3
resultOfEx3 = a `div` (length xs)
    where 
        a = 10
        xs = [1,2,3,4,5]

-- 4
ownLast ns = head (reverse ns)

-- 5

ownInit1 ns = take ((length ns) -1 ) ns
ownInit2 ns = reverse (drop 1 (reverse ns))