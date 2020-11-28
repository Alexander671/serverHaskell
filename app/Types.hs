{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Text
import Data.Aeson hiding (Result)
import Data.Time (Day)
import           GHC.Generics             hiding (from )
import Data.Vector (Vector)
import Data.ByteString (ByteString)
{-----------------------}

data StatusInsert a  = StatusInsert 
  { ok1 :: Bool,
    result1 :: Maybe (),
    error_description1 :: Maybe String,
    error_id1 :: Maybe Integer
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (StatusInsert a) where
instance ToJSON a => ToJSON (StatusInsert a) where

data Status a  = Status 
  { ok :: Bool,
    result :: Maybe (Vector a),
    error_description :: Maybe String,
    error_id :: Maybe Integer,
    role :: String
  } deriving (Show, Generic)

instance FromJSON a => FromJSON (Status a) where
instance ToJSON a => ToJSON (Status a) where

data UsersEncoder = UsersEncoder
  { image_en               :: Maybe Text,
    date_of_create_user_en :: Day, 
    first_name_en          :: Text,
    second_name_en         :: Text,
    user_id_en             :: Integer
  } deriving (Show, Generic)

instance FromJSON UsersEncoder where                            
instance ToJSON UsersEncoder where

data News = News 
  { name :: Maybe Text,
    category :: Maybe (Category),
    tags :: Maybe (Vector (Maybe Tags)),
    text_of_new :: Maybe Text,
    id_of_new :: Integer,
    autor_id :: Maybe Autors,
    images :: Maybe (Vector (Maybe Images)),
    date_of_create_new :: Maybe Day,
    comments :: Maybe (Vector (Maybe Comments)),
    photo :: Maybe Text,
    publish :: Bool
  } deriving (Show, Generic)

instance FromJSON News where
instance ToJSON News where

data NewsNotNested = NewsNotNested 
  { name_not :: Maybe Text,
    text_of_new_not :: Maybe Text,
    autor_id_not :: Maybe Integer,
    id_of_new_not :: Maybe Integer,
    date_of_create_new_not :: Maybe Day,
    category_not :: Maybe Integer,
    tags_not :: Maybe (Vector Integer),
    photo_not :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON NewsNotNested where
instance ToJSON NewsNotNested where

data NewsEncoder = NewsEncoder 
  { name_en :: Text,
    category_en :: Integer,
    tags_en :: Vector Tags,
    text_of_new_en :: Maybe Text,
    id_of_new_en :: Integer,
    autor_id_en :: Integer,
    date_of_create_new_en :: Maybe Day,
    photo_en :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON NewsEncoder where
instance ToJSON NewsEncoder where

data Autors = Autors
   { description :: Maybe Text,
     news_id :: Maybe [Maybe Integer],
     user_id_autors :: Users   
   } deriving (Show, Generic)

instance FromJSON Autors where
instance ToJSON Autors where

data AutorsEncoder = AutorsEncoder
   { description_en :: Maybe Text,
     user_id_autors_en :: Integer   
   } deriving (Show, Generic)

instance FromJSON AutorsEncoder where
instance ToJSON AutorsEncoder where


data Users = Users
  { date_of_create_user :: Day, 
    first_name          :: Maybe Text,
    second_name         :: Maybe Text,
    user_id             :: Integer,
    image               :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Users where                            
instance ToJSON Users where

data Comments = Comments
  { user_id_comment :: Users,
    id_of_comment   :: Integer,
    id_of_new_comment :: Integer,
    text_of_comment :: Text
  } deriving (Show, Generic)

instance FromJSON Comments where                            
instance ToJSON Comments where

data CommentsNotNested = CommentsNotNested
  { id_of_comment_nt   :: Integer,
    user_id_comment_nt :: Integer,
    id_of_new_comment_nt :: Integer,
    text_of_comment_nt :: Text
  } deriving (Show, Generic)

instance FromJSON CommentsNotNested where                            
instance ToJSON CommentsNotNested where

data Draft = Draft
  { id_of_user    :: Integer,
    text_of_draft :: Maybe Text,
    id_of_draft   :: Integer,
    category_id_draft :: Integer,
    photo_draft   :: Maybe Text
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

data Images = Images
  { image_new    :: Integer,
    id_of_image  :: Text
  } deriving (Show, Generic)

instance FromJSON Images where                            
instance ToJSON Images where

data LogInUp = LogInUp
  { user_id_reg :: Integer,
    login :: Text,
    password :: Text,
    token :: Text
  } deriving (Show,Generic)

instance FromJSON LogInUp where                            
instance ToJSON LogInUp where


newtype Integer1 = Integer1
  {
    int :: Integer
  } deriving (Show,Generic)

instance FromJSON Integer1 where                            
instance ToJSON Integer1 where

newtype Text1 = Text1
  {
    txt :: Text
  } deriving (Show,Generic)

instance FromJSON Text1 where                            
instance ToJSON Text1 where