q1 [] = []
q1 (x : xs) = q1 xs ++ [x]

