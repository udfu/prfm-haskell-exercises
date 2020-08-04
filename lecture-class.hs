type Pair a = (a,a)

safediv :: Int -> Int -> Maybe Int 
safediv _ 0 = Nothing
safediv m n = Just (m `div` n) 

safehead :: [a] -> Maybe a 
safehead [] = Nothing 
safehead xs = Just (head xs)

data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int 
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

int2nat :: Int -> Nat 
int2nat 0 = Zero 
int2nat n = Succ (int2nat (n -1))

add Zero n = n 
add (Succ m) n = Succ (add m n)

