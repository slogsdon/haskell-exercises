module Main where

import Control.Monad      (liftM)
import Numeric            (readFloat)
import System.Environment (getArgs)

import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr $ head args)

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseLists

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ case first:rest of
             "#t" -> Bool True
             "#f" -> Bool False
             atom -> Atom atom

parseCharacter :: Parser LispVal
parseCharacter = do
  c <- letter
  return $ Character c

parseDottedList :: Parser LispVal
parseDottedList = do
  head' <- endBy parseExpr spaces
  tail' <- char '.' >> spaces >> parseExpr
  return $ DottedList head' tail'

parseFloat :: Parser LispVal
parseFloat = do
  integral <- many1 digit
  _ <- char '.'
  fractional <- many1 digit
  return $ (Float . fst . head . readFloat) $ integral ++ "." ++ fractional

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseLists :: Parser LispVal
parseLists = do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (letter
         <|> space
         <|> quotedString
         <|> carriageReturn
         <|> lineFeed
         <|> tabStop
         <|> backslash)
  _ <- char '"'
  return $ String x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err   -> "No match: " ++ show err
                   Right _val -> "Found value"

carriageReturn :: Parser Char
carriageReturn = char '\\' >> char 'r'

lineFeed :: Parser Char
lineFeed = char '\\' >> char 'n'

tabStop :: Parser Char
tabStop = char '\\' >> char 't'

backslash :: Parser Char
backslash = char '\\' >> char '\\'

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

quotedString :: Parser Char
quotedString = char '\\' >> char '"'
