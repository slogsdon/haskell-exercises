module Euler.Problem1Spec ( spec ) where

import Helper
import Euler.Problem1

spec :: Spec
spec = do
  describe "sum'" $ do
    it "1 to 5" $ do
      (sum' 6 :: Int) `shouldBe` (8 :: Int)

    it "1 to 9" $ do
      (sum' 10 :: Int) `shouldBe` (23 :: Int)