module Euler.Problem3Spec ( spec ) where

import Helper
import Euler.Problem3

spec :: Spec
spec = do
  describe "primeFactors" $ do
    it "of 3" $ do
      (primeFactors 3 :: [Int]) `shouldBe` ([3] :: [Int])

    it "of 6" $ do
      (primeFactors 6 :: [Int]) `shouldBe` ([2,3] :: [Int])

    it "of 8" $ do
      (primeFactors 8 :: [Int]) `shouldBe` ([2,2,2] :: [Int])

  describe "largestPrimeFactor" $ do
    it "of 3" $ do
      (largestPrimeFactor 3 :: Int) `shouldBe` (3 :: Int)

    it "of 6" $ do
      (largestPrimeFactor 6 :: Int) `shouldBe` (3 :: Int)

    it "of 8" $ do
      (largestPrimeFactor 8 :: Int) `shouldBe` (2 :: Int)