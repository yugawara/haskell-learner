module MyStuff  where


data Tree a = Node a [Tree a] deriving (Show, Eq)
instance Functor Tree where
    fmap f  (Node n ts) =
        Node (f n) (map (fmap f) ts)

