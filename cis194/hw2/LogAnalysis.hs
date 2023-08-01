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
    | time1 < time2 = Node (insert lm left) lm2 right
    | otherwise = Node (insert lm left) lm2 right
insert _ tree  = tree

build :: [LogMessage] -> MessageTree
build  = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right

over50Error :: LogMessage -> Bool
over50Error (LogMessage (Error sev) _ _)  | sev > 50 = True
over50Error _ = False

logMessageToString :: LogMessage -> String
logMessageToString (LogMessage (Error _) _ str) = str
logMessageToString _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls =  map logMessageToString (filter over50Error (inOrder (build ls)))
