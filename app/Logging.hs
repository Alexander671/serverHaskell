{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Logging where

import Data.Text (Text)
data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text deriving (Eq, Show, Read, Ord)



