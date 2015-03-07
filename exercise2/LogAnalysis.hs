{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseList :: [String] -> LogMessage
parseList ("E":b:c:d) = LogMessage (Error (read b)) (read c) (unwords d)
parseList ("I":b:c) = LogMessage Info (read b) (unwords c)
parseList ("W":b:c) = LogMessage Warning (read b) (unwords c)
parseList xs = Unknown (unwords xs)

parseMessage :: String -> LogMessage
parseMessage = parseList . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ mt _) (Node l n@(LogMessage _ nt _) r)
                                           | mt <= nt = Node (insert m l) n r
                                           | mt > nt = Node l n (insert m r)

build :: [LogMessage] -> MessageTree
build [x] = insert x Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node l m Leaf) = inOrder l ++ [m]
inOrder (Node Leaf m r) = [m] ++ inOrder r
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map extractString $
                   filter severityPred $
                   inOrder $
                   build xs

severityPred :: LogMessage -> Bool
severityPred (LogMessage (Error severity) _ _)
                          | severity >= 50 = True
                          | otherwise = False
severityPred LogMessage{} = False

extractString :: LogMessage -> String
extractString (LogMessage _ _ string) = string
