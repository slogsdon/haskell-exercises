module Parser.Types.LispVal where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float

instance Show LispVal
  where
    show = showVal

showVal :: LispVal -> String
showVal (String s)       = "\"" ++ s ++ "\""
showVal (Character c)    = "'" ++ [c] ++ "'"
showVal (Atom a)         = a
showVal (Number n)       = show n
showVal (Float f)        = show f
showVal (Bool True)      = "#t"
showVal (Bool False)     = "#f"
showVal (List l)         = "(" ++ unwordsList l ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h
                        ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal
