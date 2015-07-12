{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words
  where parseMessage' ("I":time:message)       = LogMessage Info (read time) $ unwords message
        parseMessage' ("W":time:message)       = LogMessage Warning (read time) $ unwords message
        parseMessage' ("E":level:time:message) = LogMessage (Error $ read level) (read time) $ unwords message
        parseMessage' message                  = Unknown $ unwords message

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node left elog right)
  | getTimeStamp logMessage > getTimeStamp elog  = Node left elog (insert logMessage right)
  | getTimeStamp logMessage <= getTimeStamp elog = Node (insert logMessage left) elog right
insert _ _ = error "Invalid insertion"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = inOrder left ++ [logMessage] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getMessage errors
  where errors = inOrder . build $ filter (\x -> severe x) messages

severe :: LogMessage -> Bool
severe (LogMessage (Error i) _ _)
  | i >= 50   = True
  | otherwise = False
severe _      = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg
getMessage _ = error "Invalid LogMessage"

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ ts _) = ts
getTimeStamp _                   = 0
