module Euler.Problem4Spec ( spec ) where

import Helper
import Euler.Problem4

spec :: Spec
spec = do
  describe "reversal" $ do
    it "1" $ do
      (reversal 1 :: Int) `shouldBe` (1 :: Int)

    it "1 to 2" $ do
      (reversal 12 :: Int) `shouldBe` (21 :: Int)

    it "1 to 3" $ do
      (reversal 132 :: Int) `shouldBe` (231 :: Int)

  describe "palindromeProducts" $ do
    it "10..20" $ do
      (palindromeProducts [10..20] :: [Int]) `shouldBe` ([121,252,272,272,323,252,323] :: [Int])

  describe "largestPalindromeProduct" $ do
    it "10..20" $ do
      (largestPalindromeProduct [10..20] :: Int) `shouldBe` (323 :: Int)