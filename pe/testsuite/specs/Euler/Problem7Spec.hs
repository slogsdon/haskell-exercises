module Euler.Problem7Spec ( spec ) where

import Helper
import Euler.Problem7

spec :: Spec
spec = do
  describe "primeNumber" $ do
    it "2nd" $ do
      (primeNumber 2 :: Int) `shouldBe` (3 :: Int)

    it "3rd" $ do
      (primeNumber 3 :: Int) `shouldBe` (5 :: Int)

    it "6th" $ do
      (primeNumber 6 :: Int) `shouldBe` (13 :: Int)