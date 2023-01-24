module MyStuff where

{-
In Hask Category:

There is a type(object) D, of kind * -> *
-}
newtype D a = D a deriving (Show, Eq)

l :: Either String String
l = Left "a"

data Tree a = Node a [Tree a]
instance Functor Tree where
    fmap = undefined