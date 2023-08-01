{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
                    ("I":time:rest) ->
                        LogMessage Info (read time) (unwords rest)

                    ("W":time:rest) ->
                        LogMessage Warning (read time) (unwords rest)

                    ("E":n:time:rest) ->
                        LogMessage (Error (read n)) (read time) (unwords rest)

                    _ -> Unknown str

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

-- Can be re-written to exclude the third pattern match, but experimenting
-- in early stages
insert :: LogMessage -> MessageTree -> MessageTree
insert lm@LogMessage {} Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ time1 _) (Node left lm2@(LogMessage _ time2 _) right)
    | time1 > time2 = Node left lm2 (insert lm right)
    | time1 < time2 = Node (insert lm left) lm2 left
    | otherwise = Node (insert lm left) lm left
insert _ tree  = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build ls = foldr insert Leaf ls

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right
