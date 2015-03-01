module Main where

import System.Environment (getArgs)

import Parser.Evaluator
import Parser.Reader

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
