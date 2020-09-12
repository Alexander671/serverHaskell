{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Text
import Data.Aeson hiding (Result)
import Data.ByteString.Char8    (ByteString)
import Data.Time (Day)
import           GHC.Generics             hiding (from )
import Data.Vector (Vector)
{-----------------------}

data Status a  = Status 
  { ok :: Bool,
    result :: Maybe (Vector a),
    error_description :: Maybe String,
    error_id :: Maybe Integer
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Status a) where
instance ToJSON a => ToJSON (Status a) where


data News = News 
  { name :: Text,
    category :: Category,
    tags :: Vector Tags,
    text_of_new :: Maybe Text,
    id_of_new :: Integer,
    autor_id :: Autors,
    date_of_create_new :: Maybe Day,
    comments :: Vector Comments
    
  } deriving (Show, Generic)

instance FromJSON News where
instance ToJSON News where

data Autors = Autors
   { description :: Maybe Text,
     news_id :: Maybe [Integer],
     user_id_autors :: Users   
   } deriving (Show, Generic)

instance FromJSON Autors where
instance ToJSON Autors where

data Users = Users
  { date_of_create_user :: Day, 
    first_name          :: Text,
    second_name         :: Text,
    user_id             :: Integer,
     image               :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Users where                            
instance ToJSON Users where

data Comments = Comments
  { id_of_comment   :: Integer,
    user_id_comment :: Integer,
    id_of_new_comment :: Integer,
    text_of_comment :: Text
  } deriving (Show, Generic)

instance FromJSON Comments where                            
instance ToJSON Comments where

data Draft = Draft
  { id_of_draft   :: Integer,
    text_of_draft :: Text,
    id_of_user    :: Integer
  } deriving (Show, Generic)
instance FromJSON Draft where                            
instance ToJSON Draft where


data Category = Category
  { parent_id      :: Maybe Integer,
    category_name  :: Text,
    category_id    :: Integer
  } deriving (Show, Generic)


instance FromJSON Category where                            
instance ToJSON Category where

data Tags = Tags
  { tag_name  :: Text,
    tag_id    :: Integer
  } deriving (Show, Generic)


instance FromJSON Tags where                            
instance ToJSON Tags where


data NewsEncoder = NewsEncoder 
  { name_en :: Text,
    category_en :: Integer,
    tags_en :: Vector Tags,
    text_of_new_en :: Maybe Text,
    id_of_new_en :: Integer,
    autor_id_en :: Integer,
    date_of_create_new_en :: Maybe Day
    
  } deriving (Show, Generic)

instance FromJSON NewsEncoder where
instance ToJSON NewsEncoder where

data AutorsEncoder = AutorsEncoder
   { description_en :: Maybe Text,
     news_id_en :: Maybe [Integer],
     user_id_autors_en :: Integer   
   } deriving (Show, Generic)

instance FromJSON AutorsEncoder where
instance ToJSON AutorsEncoder where

data UsersEncoder = UsersEncoder
  { image_en               :: Maybe Text,
    date_of_create_user_en :: Day, 
    first_name_en          :: Text,
    second_name_en         :: Text,
    user_id_en             :: Integer
  } deriving (Show, Generic)

instance FromJSON UsersEncoder where                            
instance ToJSON UsersEncoder where




