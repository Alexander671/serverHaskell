{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Text
import Data.Aeson
import           GHC.Generics             hiding (from )
{-----------------------}
data News = News 
  { name :: Text,
    data_of_create :: Maybe Text,
    autor_id :: Integer,
    category :: Maybe Text,
    tags :: Maybe [Text],
    text_of_new :: Maybe Text,
    id_of_new :: Integer
  } deriving (Show, Generic)

instance FromJSON News where
instance ToJSON News where