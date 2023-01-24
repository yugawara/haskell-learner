module ReverseSpec where

import Reverse
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "myReverse" $ do
    it "handles empty lists" $ myReverse [] `shouldBe` ([] :: [Int])
    it "reverses hello" $ myReverse "hello" `shouldBe` "olleh"
    prop "double reverse is id" $ \list ->
      myReverse (betterReverse list) `shouldBe` (list :: [Int])

  describe "compare with Data.List reverse" $ do
    prop "vectorReverse" $ \list ->
      vectorReverse list `shouldBe` (reverse (list :: [Int]))
    prop "svectorReverse" $ \list ->
      svectorReverse list `shouldBe` (reverse (list :: [Int]))
    prop "uvectorReverse" $ \list ->
      uvectorReverse list `shouldBe` (reverse (list :: [Int]))