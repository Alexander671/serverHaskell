{-# LANGUAGE OverloadedStrings,  DeriveGeneric #-}
module Logging.Level where

import Data.Aeson.Types (FromJSON,ToJSON)
import GHC.Generics (Generic)

data LogLevel = INFO | DEBUG | WARN |  ERROR | FATAL deriving (Show,Read,Generic,Eq,Ord)
instance FromJSON LogLevel where
instance ToJSON LogLevel where

