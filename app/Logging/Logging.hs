{-# LANGUAGE OverloadedStrings,  DeriveGeneric #-}
module Logging.Logging 
    (runLog,
     LogLevel () ) where

import Logging.Level (LogLevel(INFO))
import Logging.File ( newFile )
import Logging.Entiry ( LogEntry(LogEntry, entryTime, entrylevel, entryProduct) )
import Data.Time.LocalTime (getZonedTime)

minimalLogLevel :: LogLevel
minimalLogLevel = INFO

runLog :: LogLevel -> String -> IO ()
runLog level data1 = do
       newFile
       timeNow <- getZonedTime
       appendFile "app/Logging/log.log" $  "\n" ++ (show $
                             LogEntry {entryTime   =timeNow,
                                       entrylevel  =level,
                                       entryProduct=data1 })
       


