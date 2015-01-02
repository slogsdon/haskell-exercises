--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem7
--
-- 10001st prime
-- Problem 7
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see 
-- that the 6th prime is 13.
--
-- What is the 10 001st prime number?
--------------------------------------------------------------------------------

module Euler.Problem7 where

import Util

primeNumber :: Int -> Int
primeNumber nth =
  last $ take nth primes
  where
    primes = 2 : _Y ( (3:) . minus [5,7..]        -- unbounded Sieve of Eratosthenes
                          . foldi (\(x:xs) ys-> x:union xs ys) [] 
                              . map (\p->[p*p, p*p+2*p..]) )
    _Y g = g (_Y g)