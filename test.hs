double x = x + x
quadruple x = double (double x)

factorial n = product [1 .. n]
average ns = sum ns `div` length ns
ownproduct[] =0
ownproduct (x :xs) = x * product xs

a = b + c
    where
        b = 1
        c = 2
d = a * 2

add (x, y) = x + y
add' x y = x + y

mult x y z = x * y * z

abs n = if n >= 0 then n else -n

signum1 n = if n < 0 then -1 else 
                if n == 0 then 0 else 1

signum2 n 
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1                

ex23 = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

ownlast ns = head (reverse ns)
owninit1 ns = reverse ( tail(reverse ns))
owninit2 ns = take ((length ns)-1) ns