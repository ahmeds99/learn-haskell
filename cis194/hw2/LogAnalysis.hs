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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) Leaf = Leaf
insert (Unknown _) (Node {}) = Node {}
insert LogMessage Leaf = 
insert LogMessage (Node MessageTree LogMessage MessageTree) = 