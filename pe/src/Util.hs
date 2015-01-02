--------------------------------------------------------------------------------
-- |
-- Module: Util
--
-- Some random functions
--------------------------------------------------------------------------------

module Util where

-- | maximum'
--
-- Finds maximum value in list
maximum' :: Ord a => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)

-- | foldt
--
-- Folds a finite list in a tree-like fashion
foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt _ z []     = z
foldt _ _ [x]    = x
foldt f z xs     = foldt f z (pairs f xs)

-- | foldi
--
-- Folds indefinitely defined list in a tree-like fashion
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi _ z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
-- | pairs
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs _ t        = t

-- | minus
--
-- Finds difference of two lists
minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

-- | union
--
-- Finds union of two lists
union :: Ord a => [a] -> [a] -> [a]
union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

-- | digits
--
-- Gets individual digits of a number into a list
digits :: Integral a => a -> [a]
digits = flip digits' [] . abs

digits' :: Integral a => a -> ([a] -> [a])
digits' n = if q == 0
  then (r :)
  else (digits q ++) . (r :)
  where
    (q, r) = n `divMod` 10