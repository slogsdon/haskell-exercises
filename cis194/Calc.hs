module Calc where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit a)   = a
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = parse . (parseExp Lit Add Mul)

parse :: Maybe ExprT -> Maybe Integer
parse Nothing     = Nothing
parse (Just expr) = Just (eval expr)

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit a                     = MinMax a
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit a                 = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul

testInteger :: String -> Maybe Integer
testInteger = testExp

testBool :: String -> Maybe Bool
testBool = testExp

testMinMax :: String -> Maybe MinMax
testMinMax = testExp

testMod7 :: String -> Maybe Mod7
testMod7 = testExp

