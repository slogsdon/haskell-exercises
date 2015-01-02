--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem10
--
-- Summation of primes
-- Problem 10
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--------------------------------------------------------------------------------

module Euler.Problem10 where

import Util

sumPrimes :: Integral a => a -> a
sumPrimes upperBound = 
  sum $ takeWhile (< upperBound) primes
  where
    primes = 2 : _Y ( (3:) . minus [5,7..]        -- unbounded Sieve of Eratosthenes
                          . foldi (\(x:xs) ys-> x:union xs ys) [] 
                              . map (\p->[p*p, p*p+2*p..]) )
    _Y g = g (_Y g)
