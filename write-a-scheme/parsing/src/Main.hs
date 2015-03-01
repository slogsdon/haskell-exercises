module Main where

import System.Environment (getArgs)

import Parser.Parser

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr $ head args)
