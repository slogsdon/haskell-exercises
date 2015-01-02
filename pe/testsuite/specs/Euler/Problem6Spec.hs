module Euler.Problem6Spec ( spec ) where

import Helper
import Euler.Problem6

spec :: Spec
spec = do
  describe "sumSquareDiff" $ do
    it "1..2" $ do
      (sumSquareDiff [1..2] :: Int) `shouldBe` (4 :: Int)

    it "1..3" $ do
      (sumSquareDiff [1..3] :: Int) `shouldBe` (22 :: Int)

    it "1..5" $ do
      (sumSquareDiff [1..10] :: Int) `shouldBe` (2640 :: Int)