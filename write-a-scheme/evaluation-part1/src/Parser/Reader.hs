module Parser.Reader where

import Text.ParserCombinators.Parsec (parse)

import Parser.Parser
import Parser.Types.LispVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err  -> String $ "No match: " ++ show err
                   Right val -> val
