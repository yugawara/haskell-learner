{-# LANGUAGE OverloadedStrings #-}
module ReverseSpec where
-- import Reverse
import Test.Hspec
import Test.Hspec.QuickCheck
import MyStuff ( State(..), Tree(..) )
import SExp
import Json
import Text.Megaparsec as M
import Data.List.NonEmpty
import qualified Data.HashMap.Strict as H

import qualified Data.ByteString as B
import JSONParser
l=MyStuff.State (\s -> (12, s))
spec :: Spec
spec = do
  -- describe "myReverse" $ do
  --   it "handles empty lists" $ myReverse [] `shouldBe` ([] :: [Int])
  --   it "reverses hello" $ myReverse "hello" `shouldBe` "olleh"
  --   prop "double reverse is id" $ \list ->
  --     myReverse (betterReverse list) `shouldBe` (list :: [Int])

  -- describe "compare with Data.List reverse" $ do
  --   prop "vectorReverse" $ \list ->
  --     vectorReverse list `shouldBe` (reverse (list :: [Int]))
  --   prop "svectorReverse" $ \list ->
  --     svectorReverse list `shouldBe` (reverse (list :: [Int]))
  --   prop "uvectorReverse" $ \list ->
  --     uvectorReverse list `shouldBe` (reverse (list :: [Int]))

  it "" $ [1] `shouldBe` [1]
  it "" $ Just 5 `shouldBe` Just 5
  it "" $ (Nothing::Maybe Int) `shouldBe` (Nothing::Maybe Int)
  it "" $ [1,2,3] `shouldBe` [1,2,3]
  it "" $ (Left "a"::Either String String) `shouldBe` (Left "a"::Either String String)
  it "" $ (Left "a"::Either String String) `shouldBe` (Left "a"::Either String String)
  it "" $ 'a' `shouldBe` 'a'
  it "" $ "a" `shouldBe` "a"
  it "" $ (1,5) `shouldBe` (1,5)
  it "" $ (1,'c') `shouldBe` (1,'c')
  it "" $ (5::Int) `shouldBe` (5::Int)
  it "" $ (5::Integer) `shouldBe` (5::Integer)
  it "" $ (5::Float) `shouldBe` (5::Float)
  it "" $ (5::Double) `shouldBe` (5::Double)

  prop "" $ \x -> ((x + 2)-2)   `shouldBe` ((x::Int) * 1)
  {-
Functor laws:
fmap id = id
fmap (f.g) == fmap f  . fmap g
  -}
  it "" $ (fmap id) (Just "A")           `shouldBe` id (Just "A")
  it "" $ (fmap ((+1) . (* 2))) (Just 1) `shouldBe`  (fmap (+1) . fmap (*2)) (Just 1)

  it "" $ (fmap id) [1]           `shouldBe` id [1]
  it "" $ (fmap ((+1) . (* 2))) [1] `shouldBe`  (fmap (+1) . fmap (*2)) [1]

  it "" $ (fmap id) ((Left 1)::Either Int String)           `shouldBe` id ((Left 1)::Either Int String)
  it "" $ (fmap ((+1) . (* 2))) (Left 1) `shouldBe`  (fmap (+1) . fmap (*2)) (Left 1)

  it "" $ (fmap id) ((Right "a")::Either Int String)           `shouldBe` id ((Right "a")::Either Int String)

  prop "" $ \x -> (fmap negate (Just x))   `shouldBe`  (Just (- (x::Int)))
  prop "" $ \x -> (negate <$> (Just x))   `shouldBe`  (Just (- (x::Int)))
  it "" $ fmap negate Nothing `shouldBe` Nothing

  it "" $ (fmap id) ((Node 1 [])::Tree Int)           `shouldBe` id ((Node 1 [])::Tree Int)
  prop "" $ \x -> (fmap ((*7) . (+ 2))) (Node x [Node 3 [], Node 14 [Node 2 []]]) `shouldBe`  (fmap (*7) . fmap (+2)) (Node (x::Int) [Node 3 [], Node 14 [Node 2 []]])

  it "" $ pure id <*> [1] `shouldBe` [1]
  it "" $ pure (+4) <*> [1] `shouldBe` pure ((+4) 1)
  it "" $ [(+1)] <*> pure 2 `shouldBe` pure ($ 2) <*> [(+1)]
  it "" $ pure (.) <*> [(+1)] <*> [(+2)] <*> [3] `shouldBe` [(+1)] <*>( [(+2)] <*> [3])
  it "" $ runState ((pure id) <*> (pure id) <*> (pure 1 )) 10 `shouldBe` runState (pure 1) 10
  it "" $ runState ((pure id) <*> (pure id) <*> (pure 1 )) 10 `shouldBe` runState (pure 1) 10
  it "" $ runState ((MyStuff.State (\x -> ((+9),x+1)))<*>(pure (5::Int))::MyStuff.State Int Int) (10::Int) `shouldBe`  (14,11)
  -- it "" $ parseTest atom "(foo -42 \"with \\\" now!\")" `shouldBe` SSExp (SId (Identifier {getId = "foo"})) [SInteger (-42),SString "with \" now!"]

  it "" $ M.runParser atom "testSource" "()" `shouldBe` Right (SSExp [])
  it "" $ M.runParser atom "testSource" "a" `shouldBe` Right (SId (Identifier "a"))
  it "" $ M.runParser atom "testSource" "a-" `shouldBe` Right (SId (Identifier "a-"))
  it "" $ parseJson  ("{\"a\":3} ":: B.ByteString) `shouldBe` Object (H.fromList [("a", Number 3.0)])

  prop "" $ \x -> (fmap (a . b)  (DD (x::Integer))) `shouldBe` ((fmap a . fmap b)  (DD (x::Integer)))
  prop "" $ \x -> (fmap (a . b)  (EE (x::Integer))) `shouldBe` ((fmap a . fmap b)  (EE (x::Integer)))
  it "" $  (fmap (a . b)  Nothing2) `shouldBe` (fmap a . fmap b)  Nothing2
a = (+ 3)
b = (* 5)