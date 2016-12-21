{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Week2.Log

parseMessage :: String -> LogMessage
parseMessage message =
    let (t:rest) = words message
        (goodTimestamp:goodRest) = rest
        (errorCode:(errorTimestamp:errorRest)) = rest
    in case t of
        "E" ->
           LogMessage
               (Error $ read errorCode)
               (read errorTimestamp)
               (unwords errorRest)
        "I" -> LogMessage Info (read goodTimestamp) (unwords goodRest)
        "W" -> LogMessage Warning (read goodTimestamp) (unwords goodRest)
        _ -> Unknown message

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert message tree =
    let
        insert_ tree_ time =
            case tree_ of
                Leaf ->
                    Node Leaf message Leaf
                Node left parentMessage right ->
                    case parentMessage of
                        Unknown _ ->
                            tree_
                        (LogMessage _ parentTime _) ->
                            if time < parentTime then
                                Node (insert_ left time) parentMessage right
                            else
                                Node left parentMessage (insert_ right time)
    in
        case message of
            Unknown _ ->
                tree
            (LogMessage _ time _) ->
                insert_ tree time


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder tree =
    case tree of
        Leaf ->
            []
        Node left message right ->
            inOrder left ++ message:inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x:xs) =
    case x of
        LogMessage (Error severity) _ message ->
            if severity > 50 then
                message:whatWentWrong xs
            else
                whatWentWrong xs
        _ ->
            whatWentWrong xs
