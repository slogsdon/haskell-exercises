module Euler.Problem2Spec ( spec ) where

import Helper
import Euler.Problem2

spec :: Spec
spec = do
  describe "fibNaive" $ do
    it "first" $ do
      (fibNaive 1 :: Int) `shouldBe` (1 :: Int)

    it "second" $ do
      (fibNaive 2 :: Int) `shouldBe` (2 :: Int)

    it "fourth" $ do
      (fibNaive 4 :: Int) `shouldBe` (5 :: Int)

  describe "fib" $ do
    it "first" $ do
      (fib 1 :: Int) `shouldBe` (1 :: Int)

    it "second" $ do
      (fib 2 :: Int) `shouldBe` (2 :: Int)

    it "fourth" $ do
      (fib 4 :: Int) `shouldBe` (5 :: Int)

  describe "fibEvenList" $ do
    it "less than 1" $ do
      (fibEvenList 1 :: [Int]) `shouldBe` ([] :: [Int])

    it "less than 4" $ do
      (fibEvenList 4 :: [Int]) `shouldBe` ([2] :: [Int])

    it "less than 10" $ do
      (fibEvenList 10 :: [Int]) `shouldBe` ([2, 8] :: [Int])

  describe "fibSum" $ do
    it "less than 1" $ do
      (fibSum 1 :: Int) `shouldBe` (0 :: Int)

    it "less than 4" $ do
      (fibSum 4 :: Int) `shouldBe` (2 :: Int)

    it "less than 10" $ do
      (fibSum 10 :: Int) `shouldBe` (10 :: Int)