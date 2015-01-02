module Intro where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n = splitDigits n []
  where
    splitDigits num acc
      | num <= 0 = acc
      | otherwise = splitDigits next (digit:acc)
      where
        digit = num `mod` 10
        next  = num `div` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = double [] False
  where
    double acc _ []         = acc
    double acc True (x:xs)  = double ((2*x):acc) False xs
    double acc False (x:xs) = double (x:acc) True xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- Exercise 4
validate :: Integer -> Bool
validate n = sum `mod` 10 == 0
  where
    sum = sumDigits $ doubleEveryOther $ toDigitsRev n

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n source dest int =
  hanoi (n - 1) source int dest ++ [(source, dest)] ++ hanoi (n - 1) int dest source
