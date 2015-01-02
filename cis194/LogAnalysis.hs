{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage ('I':xs) = LogMessage Info (basicTimestamp xs) (message xs)
parseMessage ('W':xs) = LogMessage Warning (basicTimestamp xs) (message xs)
parseMessage ('E':xs) = LogMessage (Error (errorCode xs)) (errorTimestamp xs) (errorMessage xs)
parseMessage xs       = Unknown xs

basicTimestamp :: String -> Int
basicTimestamp = read . head . words

errorTimestamp :: String -> Int
errorTimestamp = read . last . take 2 . words

errorCode :: String -> Int
errorCode = read . head . words

message :: String -> String
message = unwords . drop 1 . words

errorMessage :: String -> String
errorMessage = unwords . drop 2 . words

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

-- Exercise 2
-- https://en.wikipedia.org/wiki/Tree_sort#Example used as
-- refence for pseudocode (ignoring haskell implementation
-- below it).
--
-- If `Ord` was implemented for `MessageTree`, this could
-- be neater by not needing to destructure the node to
-- grab the timestamp from it (or a separate function to
-- which to offload this).
insert :: LogMessage -> MessageTree -> MessageTree
insert mes Leaf         = Node Leaf mes Leaf
insert (Unknown _) tree = tree
insert mes@(LogMessage _ time _) (Node ltree node@(LogMessage _ nodeTime _) rtree)
  | time <= nodeTime = Node (insert mes ltree) node rtree
  | otherwise        = Node ltree node (insert mes rtree)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build lst = build' lst Leaf
  where
    build' :: [LogMessage] -> MessageTree -> MessageTree
    build' [] tree     = tree
    build' (x:xs) tree = build' xs (insert x tree)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                    = []
inOrder (Node ltree node rtree) =
  inOrder ltree ++ [node] ++ inOrder rtree

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs =
  map getLogMessage $ filter errorWithSeverity50OrGreater $ inOrder (build xs)

errorWithSeverity50OrGreater :: LogMessage -> Bool
errorWithSeverity50OrGreater (LogMessage (Error level) _ _)
  | level >= 50 = True
  | otherwise   = False
errorWithSeverity50OrGreater _ = False

getLogMessage :: LogMessage -> String
getLogMessage (LogMessage _ _ mes) = mes
getLogMessage (Unknown mes)        = mes
