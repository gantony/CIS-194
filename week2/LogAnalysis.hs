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

--isUnknown :: LogMessage -> Bool
--isUnknown (Unknown _) = True
--isUnknown _ = False

--getTimestamp :: MessageTree -> TimeStamp
--getTimestamp (Node leftTree (LogMessage _ timestamp _) rightTree) = timestamp

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage (Leaf) = (Node Leaf logMessage Leaf)
insert (LogMessage logMT timestamp logString) (Node leftTree (LogMessage nodeMT nodeTimestamp nodeString) rightTree) 
     | timestamp < nodeTimestamp = (Node (insert (LogMessage logMT timestamp logString) leftTree) (LogMessage nodeMT nodeTimestamp nodeString) rightTree)
     | otherwise                 = (Node leftTree (LogMessage nodeMT nodeTimestamp nodeString) (insert (LogMessage logMT timestamp logString) rightTree))
insert _ (Node _ (Unknown _) _) = error "Invalid Tree!" 

