module Parser.Evaluator where

import Parser.Types.LispVal

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args))     = apply f $ map eval args
eval val                        = val

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isAtom),
              ("string?", isString),
              ("number?", isNumber)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0

isAtom :: [LispVal] -> LispVal
isAtom vals = Bool $ foldl1 (&&) $ map isAtom' vals
  where
    isAtom' :: LispVal -> Bool
    isAtom' (Atom _) = True
    isAtom' _        = False

isString :: [LispVal] -> LispVal
isString vals = Bool $ foldl1 (&&) $ map isString' vals
  where
    isString' :: LispVal -> Bool
    isString' (String _) = True
    isString' _          = False

isNumber :: [LispVal] -> LispVal
isNumber vals = Bool $ foldl1 (&&) $ map isNumber' vals
  where
    isNumber' :: LispVal -> Bool
    isNumber' (Number _) = True
    isNumber' _          = False
