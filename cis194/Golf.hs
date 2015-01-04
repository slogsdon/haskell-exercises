module Golf where

import Data.List (nub)

-- Exercise 1: Hopscotch
skips :: [a] -> [[a]]
skips xs =
  -- For each index `n` in our list,
  map (\n ->
    -- we reduce `xs` by selecting every `n`th element and
    -- prepending it onto an accumulator.
    foldr (\(i,x) acc -> if i`mod`n==0 then x:acc else acc)
          -- Our accumulator.
          []
          -- Create (index,letter) pairs to compare index
          -- against the current value of `n`.
          (zip [1..] xs))
    -- Our list, i.e. integers from 1 to the length of `xs`
    [1..(length xs)]

-- Exercise 2: Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima = lMax' []
  where
    lMax' :: [Integer] -> [Integer] -> [Integer]
    -- Deconstruct the list to ensure we only deal with
    -- triplets (a number `b` has to has to have a number
    -- before `a` and a number after it `c`).
    lMax' acc (a:xs@(b:c:_))
      -- When `b` is greater than both `a` and `c`,
      -- append `b` to our accumulator.
      | a<b && b>c  = lMax' (acc++[b]) xs
      -- Otherwise, we continue calculating with the rest
      -- of the list.
      | otherwise   = lMax' acc xs
    -- All other cases pass the current value of the
    -- accumulator. Occurs when the input list is too
    -- short or when we've reached the end of it.
    lMax' acc _ = acc

-- Exercise 3: Histogram
histogram :: [Integer] -> String
histogram xs =
  -- separator row
  replicate (length s) '=' ++ "\n"
  -- legend row
  ++ foldl (\acc x -> acc++show x) "" s ++ "\n"
  where s = nub xs

