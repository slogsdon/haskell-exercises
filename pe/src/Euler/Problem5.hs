--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem5
--
-- Smallest multiple 
-- Problem 5
--
-- 2520 is the smallest number that can be divided by each of the numbers from 
-- 1 to 10 without any remainder.
--
-- What is the smallest positive number that is evenly divisible by all of the 
-- numbers from 1 to 20?
--------------------------------------------------------------------------------

module Euler.Problem5 where

-- | smallestMultiple
--
-- finds smallest multipl of nums
smallestMultiple :: Integral n => [n] -> n
smallestMultiple nums =
  number 1
  where
    filteredDivisorsOf x = filter (\n -> x `mod` n == 0)
    number x
      | filteredDivisorsOf x nums == nums = x
      | otherwise = number (x+1)