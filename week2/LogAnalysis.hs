{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseInfo :: [String] -> LogMessage
parseInfo (ts:text) = LogMessage Info (read ts::Int) (unwords text)
parseInfo _ = Unknown "unknown"

parseWarning :: [String] -> LogMessage
parseWarning (ts:text) = LogMessage Warning (read ts::Int) (unwords text)
parseWarning _ = Unknown "unknown"

parseError :: [String] -> LogMessage
parseError (es:ts:text) = LogMessage (Error (read es::Int)) (read ts::Int) (unwords text)
parseError _ = Unknown "unknown"

parseMessage :: String -> LogMessage
parseMessage text =
    case messageType of
        "I" -> parseInfo messageDetails
        "W" -> parseWarning messageDetails
        "E" -> parseError messageDetails
        _   -> Unknown "unknown"
    where messageType = head messageWords
          messageDetails = tail messageWords
          messageWords = words text

parse :: String -> [LogMessage]
parse text = map parseMessage (lines text)


logTimestampOf :: LogMessage -> Maybe TimeStamp
logTimestampOf (Unknown _) = Nothing
logTimestampOf (LogMessage _ timestamp _) = Just timestamp

nodeTimestampOf :: MessageTree -> Maybe TimeStamp
nodeTimestampOf (Node _ (LogMessage _ timestamp _) _) = Just timestamp
nodeTimestampOf _ = Nothing

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage (Leaf) = (Node Leaf logMessage Leaf)
insert logMessage (Node leftTree nodeMessage rightTree) 
     | timestamp < nodeTimestamp = (Node (insert logMessage leftTree) nodeMessage rightTree)
     | otherwise                 = (Node leftTree nodeMessage (insert logMessage rightTree))
    where timestamp     = logTimestampOf logMessage
          nodeTimestamp = nodeTimestampOf (Node leftTree nodeMessage rightTree)


build :: [LogMessage] -> MessageTree
build list = foldl (flip insert) (Leaf) list 


inOrder :: MessageTree -> [LogMessage]
inOrder (Node leftTree nodeMessage rightTree) = (inOrder leftTree) ++ [nodeMessage] ++ (inOrder rightTree)
inOrder (Leaf) = []


highPriority :: LogMessage -> Bool
highPriority (LogMessage (Error severity) _ _) = severity > 50
highPriority _ = False

errorToString :: LogMessage -> String
errorToString (LogMessage (Error _) _ text) = show text 
errorToString _ = error "Only expecting error messages"

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map errorToString highPriorityMessages
    where messagesInOrder = inOrder (build messages) 
          highPriorityMessages = filter (highPriority) messagesInOrder
