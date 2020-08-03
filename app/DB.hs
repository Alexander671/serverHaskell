{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module DB where
import Types
{------------------------------------}
import qualified Hasql.Connection as HC 
import qualified Hasql.Session as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Statement as HST
{------------------------------------}
import Data.Text 
import Network.URI.Encode (decodeBSToText,encodeTextToBS)
{------------------------------------}
import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
{------------------------------------}
import qualified Control.Monad
import Data.Foldable                      as DF
{------------------------------------}
import Data.Functor.Contravariant 
{------------------------------------}


connectToDB :: BS.ByteString -> HD.Result [a] -> IO [a]
connectToDB query dec =
  let
    connectionSettings :: HC.Settings
    connectionSettings =
      HC.settings
        "localhost"
        (fromInteger 5432)
        "postgres"
        "password"
        ""
  in do
    connectionResult <- HC.acquire connectionSettings
    case connectionResult of
      Left (Just errMsg) -> error $ BS.unpack errMsg
      Left Nothing -> error "Unspecified connection error"
      Right connection ->  do
        putStrLn "Acquired connection!"
        queryResult <- HS.run (selectTasksSession query dec) connection
        HC.release connection
        case queryResult of
          Right result -> return result
          Left err -> error $ show err


selectTasksSession :: BS.ByteString -> HD.Result [a]  -> HS.Session [a]
selectTasksSession query dec = HS.statement () $ selectTasksStatement query dec

selectTasksStatement :: BS.ByteString -> HD.Result [a]  -> HST.Statement () [a]
selectTasksStatement query dec =
  HST.Statement
    query
    HE.noParams
    dec
    True


{-Encoder for type News-}
someNewsEncoder :: HE.Params News
someNewsEncoder = 
          (name >$< HE.param (HE.nonNullable HE.text)) <>
          (date_of_create_new >$< HE.param (HE.nullable HE.date)) <>
          (fromIntegral . autor_id >$< HE.param (HE.nonNullable HE.int8)) <>
          (category >$< HE.param (HE.nullable HE.text)) <>
          (tags >$< HE.param (HE.nullable $ HE.array $ HE.dimension DF.foldl' $ HE.element $ HE.nonNullable HE.text)) <>
          (text_of_new >$< HE.param (HE.nullable HE.text)) <>
          (fromIntegral . id_of_new >$< HE.param (HE.nonNullable HE.int8)) 

{-Decoder-}
someNewsDecoder :: HD.Result [News]
someNewsDecoder =  HD.rowList $ Types.News <$> 
          (HD.column (HD.nonNullable HD.text)) <*>
          (HD.column (HD.nullable HD.date)) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (HD.column $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nonNullable HD.text))) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8)))
          
{-Encoder for type Autors-}
someAutorsEncoder :: HE.Params Autors
someAutorsEncoder =  
          (fromIntegral . user_id_autors >$< HE.param (HE.nonNullable HE.int8)) <>
          (description >$< HE.param (HE.nullable HE.text)) <>
          (news_id >$< HE.param (HE.nullable $ HE.array $ HE.dimension DF.foldl' $ HE.element $ HE.nonNullable $ contramap fromIntegral HE.int8))

{-Decoder-}
someAutorsDecoder :: HD.Result [Autors]
someAutorsDecoder = HD.rowList $ Types.Autors <$>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (HD.column $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nonNullable (fromIntegral <$> HD.int8)))) 
          
{-Encoder for type Users-}
someUsersEncoder :: HE.Params Users
someUsersEncoder = 
          (image >$< HE.param (HE.nullable $ contramap encodeTextToBS HE.bytea)) <>
          (date_of_create_user >$< HE.param (HE.nonNullable HE.date)) <>
          (admin >$< HE.param (HE.nonNullable $  HE.bool)) <>
          (fromIntegral . user_id >$< HE.param (HE.nonNullable HE.int8)) <>
          (first_name >$< HE.param (HE.nonNullable HE.text)) <>
          (second_name >$< HE.param (HE.nonNullable HE.text)) 

{-Decoder-}
someUsersDecoder :: HD.Result [Users]
someUsersDecoder = HD.rowList $ Types.Users <$>
          (HD.column (HD.nullable $ decodeBSToText <$> HD.bytea)) <*>
          (HD.column (HD.nonNullable HD.date)) <*>
          (HD.column (HD.nonNullable HD.bool)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nonNullable HD.text)) <*>
          (HD.column (HD.nonNullable HD.text))
          
{-Encoder for type Comment-}
someCommentsEncoder :: HE.Params Comments                      
someCommentsEncoder = 
          (text_of_comment >$< HE.param (HE.nonNullable HE.text)) <>
          (fromIntegral . user_id_comment >$< HE.param (HE.nonNullable HE.int8)) <>
          (fromIntegral . id_of_comment >$< HE.param (HE.nonNullable HE.int8))

{-Decoder-}
someCommentsDecoder :: HD.Result [Comments]
someCommentsDecoder = HD.rowList $ Types.Comments <$>
          (HD.column (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) 
          
            