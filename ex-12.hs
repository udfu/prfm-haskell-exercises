-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    fmap g (Leaf) = Leaf 
    fmap g (Node x y z) = Node (fmap g x) (g y) (fmap g z)

-- fmap length (Node (Leaf) "abc" (Leaf))

-- 2
