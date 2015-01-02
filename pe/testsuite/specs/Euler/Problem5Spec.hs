module Euler.Problem5Spec ( spec ) where

import Helper
import Euler.Problem5

spec :: Spec
spec = do
  describe "smallestMultiple" $ do
    it "1..2" $ do
      (smallestMultiple [1..2] :: Int) `shouldBe` (2 :: Int)

    it "1..3" $ do
      (smallestMultiple [1..3] :: Int) `shouldBe` (6 :: Int)

    it "1..5" $ do
      (smallestMultiple [1..5] :: Int) `shouldBe` (60 :: Int)