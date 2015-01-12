module Fibonacci where

import Data.Function (fix)

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2
fib' :: (Int -> Integer) -> Int -> Integer
fib' _ 0 = 0
fib' _ 1 = 1
fib' f n = f (n-1) + f (n-2)

memoize :: (Int -> a) -> Int -> a
memoize f = (map f [0..] !!)

fibMemo :: Int -> Integer
fibMemo = fix (memoize . fib')

fibs2 :: [Integer]
fibs2 = map fibMemo [1..]

-- Exercise 3
data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (Cons a s1) s2 = Cons a (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = streamMap (\x->powerOf2 x x) (streamFromSeed (+1) 1)

powerOf2 :: Integer -> Integer -> Integer
powerOf2 _ 0 = 0
powerOf2 n e
  | n `mod` 2^e == 0 = e
  | otherwise        = powerOf2 n (e-1)

