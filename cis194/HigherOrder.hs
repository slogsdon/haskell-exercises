module HigherOrder where

import Data.List (sort)

-- Exercise 1: Wholemeal programming

-- fun1 :: [Integer] -> Integer
-- fun1 []       = 1
-- fun1 (x:xs)
--   | even x    = (x - 2) * fun1 xs
--   | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . subtract2 . evensOnly

evensOnly :: [Integer] -> [Integer]
evensOnly = filter even

subtract2 :: [Integer] -> [Integer]
subtract2 = map (\x -> x - 2)

-- fun2 :: Integer -> Integer
-- fun2 1             = 0
-- fun2 n | even n    = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' 1             = 0
fun2' n | even n    = n + (fun2' . intDivBy2) n
        | otherwise = (fun2' . add1 . multiplyBy3) n

intDivBy2 :: Integer -> Integer
intDivBy2 n = n `div` 2

add1 :: Integer -> Integer
add1 = (+ 1)

multiplyBy3 :: Integer -> Integer
multiplyBy3 = (* 3)

-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = buildTree . sort

buildTree :: [a] -> Tree a
buildTree xs = build xs 0 (length xs)
  where
    build :: [a] -> Int -> Int -> Tree a
    build ys mn mx
      | mn >= mx  = Leaf
      | otherwise = Node level ltree node rtree
        where
          mdn   = mn + (mx - mn) `div` 2
          level = nearestPowerOf2 (mx-mn)
          ltree = build ys mn mdn
          node  = head (drop mdn ys)
          rtree = build ys mdn mx

nearestPowerOf2 :: Int -> Int
nearestPowerOf2 = nearest 2
  where
    nearest :: Int -> Int -> Int
    nearest p n
      | p > n = p
      | otherwise = nearest (2*p) n

-- Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = odd . length . foldr (\x acc -> if x then x:acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

