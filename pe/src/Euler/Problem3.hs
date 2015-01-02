--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem3
--
-- Largest prime factor
-- Problem 3
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?
--------------------------------------------------------------------------------

module Euler.Problem3 where

import Util

-- | largestPrimeFactor
--
-- finds prime factor of n
largestPrimeFactor :: Integral n => n -> n
largestPrimeFactor n = maximum' $ primeFactors n

-- | primeFactors
--
-- lists prime factors of n
primeFactors :: Integral n => n -> [n]
primeFactors num =
  case factors of
    [] -> [num]
    _  -> factors ++ primeFactors (num `div` (head factors))
  where
    factors = take 1 $ filter (\x -> num `mod` x == 0) [2 .. num-1]  