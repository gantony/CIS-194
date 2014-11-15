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