--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem2
--
-- Even Fibonacci numbers
-- Problem 2
--
-- Each new term in the Fibonacci sequence is generated by adding the previous 
-- two terms. By starting with 1 and 2, the first 10 terms will be:
-- 
-- > 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
-- By considering the terms in the Fibonacci sequence whose values do not exceed 
-- four million, find the sum of the even-valued terms.
--------------------------------------------------------------------------------

module Euler.Problem2 where

-- | fibSum
--
-- finds some of even fionacci numbers less than n
fibSum :: Integral n => n -> n
fibSum n =
  sum $ fibEvenList n

-- | fibEvenList
--
-- returns list of fibonacci numbers less than n
fibEvenList :: Integral n => n -> [n]
fibEvenList n = 
  filter (\x -> x `mod` 2 == 0) numbers
  where
    numbers2 x
      | num < n = [num] ++ numbers2 (x + 1)
      | otherwise = []
      where
        num = fib x
    numbers= numbers2 1

fib :: Integral n => n -> n
fib = fst . fib2
 
-- | Return (fib n, fib (n + 1))
fib2 :: Integral n => n -> (n, n)
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b

-- | fibNaive
--
-- returns fibonacci number n
fibNaive :: Integral n => n -> n
fibNaive 1 = 1
fibNaive 2 = 2
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)
