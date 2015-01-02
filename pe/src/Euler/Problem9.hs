--------------------------------------------------------------------------------
-- |
-- Module: Euler.Problem9
--
-- Special Pythagorean triplet
-- Problem 9
--
-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which
--
-- > a^2 + b^2 = c^2
--
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
--------------------------------------------------------------------------------

module Euler.Problem9 where

specialTriplet :: Integral a => a -> a
specialTriplet s = 
  head list
  where
    list = [a*b*c | c <- [2..], b <- [2..c-1], a <- [2..b-1], (isTriplet a b c) && (a+b+c==s)]

isTriplet :: Integral a => a -> a -> a -> Bool
isTriplet a b c = (a < b && b < c) && (a*a + b*b == c*c)
