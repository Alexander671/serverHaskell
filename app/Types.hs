{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Text
import Data.Aeson
import Data.ByteString.Char8    (ByteString)
import Data.Time (Day)
import           GHC.Generics             hiding (from )
{-----------------------}

data News = News 
  { name :: Text,
    date_of_create_new :: Maybe Day,
    category :: Maybe Text,
    tags :: Maybe [Text],
    text_of_new :: Maybe Text,
    id_of_new :: Integer,
    autor_id :: Integer
    
  } deriving (Show, Generic)

instance FromJSON News where
instance ToJSON News where

data Autors = Autors
   { user_id_autors :: Integer,
     description :: Maybe Text,
     news_id :: Maybe [Integer]
   } deriving (Show, Generic)

instance FromJSON Autors where
instance ToJSON Autors where

data Users = Users
  { image               :: Maybe Text,
    date_of_create_user :: Day, 
    admin               :: Bool,
    user_id             :: Integer,
    first_name          :: Text,
    second_name         :: Text
  } deriving (Show, Generic)

instance FromJSON Users where                            
instance ToJSON Users where

data Comments = Comments
  { text_of_comment :: Text,
    user_id_comment :: Integer,
    id_of_comment   :: Integer
  } deriving (Show, Generic)

instance FromJSON Comments where                            
instance ToJSON Comments where

data Draft = Draft
  { id_of_draft   :: Integer,
    id_of_user    :: Integer,
    text_of_draft :: Text
  } deriving (Show, Generic)
instance FromJSON Draft where                            
instance ToJSON Draft where