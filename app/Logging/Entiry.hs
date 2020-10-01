{-# LANGUAGE DeriveGeneric #-}

module Logging.Entiry where
import Data.Time.LocalTime (ZonedTime, LocalTime)
import Logging.Level ( LogLevel )
import GHC.Generics (Generic)
import Data.Aeson (FromJSON,ToJSON)


data LogEntry =
  LogEntry { -- A local time contains the date and the time of the day.
             -- For example: 2013-06-29 11:16:23.
              entryTime      :: ZonedTime
            -- INFO, DEBUG ... etc
            , entrylevel     :: LogLevel
            , entryProduct   :: String
             } deriving (Show,Read,Generic)

instance ToJSON   LogEntry where
instance FromJSON LogEntry where
