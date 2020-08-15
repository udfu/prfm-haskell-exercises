data Tree n l = Leaf l | Node n (Tree n l) (Tree n l)

-- 2.1
equals :: (Eq n, Eq l) => Tree n l -> Tree n l -> Bool
equals (Leaf a) (Leaf b) | a == b = True
                         | otherwise = False
equals (Node n x y) (Node m a b) = n == m && equals x a && equals y b

-- 2.2
instance (Eq n, Eq l) => Eq (Tree n l) where
    (==) = equals


-- 3.1
showInorder (Leaf x) = show x
showInorder (Node n x y) = showInorder x ++ show n ++ showInorder y

-- 3.2
-- instance Show (Tree n l) => Show (Tree n l) 


-- 4
eval (Leaf x) = x
eval (Node n x y) = n (eval x) (eval y)


-- 5
member x (Leaf y) = x == y
member x (Node n y z) = n == x || member x y || member x z 
