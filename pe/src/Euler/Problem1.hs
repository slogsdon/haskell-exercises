--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem1
--
-- Multiples of 3 and 5
-- Problem 1
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we 
-- get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
--------------------------------------------------------------------------------

module Euler.Problem1 where

-- | sum'
--
-- finds sum of all multiples of 3 & 5 which are below than n
sum' :: Integral n => n -> n
sum' n =
  sum numbers
  where
    numbers = [x | x <- [1..n-1], x `mod` 3 == 0 || x `mod` 5 == 0]