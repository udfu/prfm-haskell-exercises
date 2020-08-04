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
--showInorder :: Tree -> String

