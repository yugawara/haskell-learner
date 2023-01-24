module ReverseSpec where

-- import Reverse
import Test.Hspec
import Data.List
import Test.Hspec.QuickCheck
import MyStuff

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

  it "" $ (D 5) `shouldBe` (D 5)
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
  it "" $ (fmap ((+1) . (* 2))) (Just 0) `shouldBe`  (fmap (+1) . fmap (*2)) (Just 0)

  it "" $ (fmap id) [1]           `shouldBe` id [1]
  it "" $ (fmap ((+1) . (* 2))) [0] `shouldBe`  (fmap (+1) . fmap (*2)) [0]

  it "" $ (fmap id) ((Left 1)::Either Int Int)           `shouldBe` id ((Left 1)::Either Int Int)
  it "" $ (fmap ((+1) . (* 2))) (Left 0) `shouldBe`  (fmap (+1) . fmap (*2)) (Left 0)

  prop "" $ \x -> (fmap negate (Just x))   `shouldBe`  (Just (- (x::Int)))
  prop "" $ \x -> (negate <$> (Just x))   `shouldBe`  (Just (- (x::Int)))
  it "" $ fmap negate Nothing `shouldBe` Nothing