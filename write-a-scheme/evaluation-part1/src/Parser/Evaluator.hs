module Parser.Evaluator where

import Parser.Types.LispVal

eval :: LispVal -> LispVal
eval (List [Atom "quote", val]) = val
eval (List [Atom f, val])       = maybe (Bool False) ($ val) $ lookup f symbols
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
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber)]

symbols :: [(String, LispVal -> LispVal)]
symbols = [("symbol->string", symbolToString),
           ("string->symbol", stringToSymbol)]

--
-- Symbolic Functions
--

isSymbol :: [LispVal] -> LispVal
isSymbol = Bool . all isAtom'
  where
    isAtom' :: LispVal -> Bool
    isAtom' (Atom _) = True
    isAtom' _        = False

symbolToString :: LispVal -> LispVal
symbolToString (Atom a) = String a
symbolToString _        = String ""

stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom s
stringToSymbol _          = Atom ""

--
-- String Functions
--

isString :: [LispVal] -> LispVal
isString = Bool . all isString'
  where
    isString' :: LispVal -> Bool
    isString' (String _) = True
    isString' _          = False

--
-- Numeric Functions
--

isNumber :: [LispVal] -> LispVal
isNumber = Bool . all isNumber'
  where
    isNumber' :: LispVal -> Bool
    isNumber' (Number _) = True
    isNumber' _          = False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params
  where
    unpackNum :: LispVal -> Integer
    unpackNum (Number n) = n
    unpackNum _          = 0
