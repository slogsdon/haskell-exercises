--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem4
--
-- Largest palindrome product
-- Problem 4
--
-- A palindromic number reads the same both ways. The largest palindrome made 
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.
--------------------------------------------------------------------------------

module Euler.Problem4 where

import Util

-- | largestPalindromeProduct
--
-- finds largest palindrome product
largestPalindromeProduct :: [Int] -> Int
largestPalindromeProduct nums =
  maximum' $ palindromeProducts nums

palindromeProducts :: [Int] -> [Int]
palindromeProducts nums =
  [ x*y | x <- nums, y <- nums, x*y == reversal (x*y)]

reversal :: Int -> Int
reversal num =
  (read reversedStr) :: Int
  where
    str = show num
    reversedStr = reverse str :: String 
