{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Vector
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
import Data.Aeson (ToJSON, encode)
import Data.Aeson (decode)

{------------------------------------}

connectToDB :: ToJSON a => BS.ByteString -> HD.Result (Vector a) -> IO (Status a)
connectToDB query dec  =
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
          Right result1 -> do
                        putStrLn $ show $ encode $ Status {ok=True,result=Just $ Result (Just result1),error_description=Nothing,error_id=Nothing}
                        return $ Status {ok=True,result=Just $ Result (Just result1),error_description=Nothing,error_id=Nothing} -- понять какой тип использовать для ошибки
          Left err -> do
                     putStrLn $ show $ encode (Status {ok=False,result=Nothing,error_description=Just $ show err,error_id=Just 404} :: Types.Status News)
                     return $ Status {ok=False,result=Nothing,error_description=Just "problem with db",error_id=Just 404}


selectTasksSession ::  BS.ByteString -> HD.Result (Vector a) -> HS.Session (Vector a)
selectTasksSession  query dec = HS.statement () (selectTasksStatement query dec) 


selectTasksStatement ::  BS.ByteString -> HD.Result (Vector a) -> HST.Statement () (Vector a)
selectTasksStatement sql dec =
             HST.Statement 
             sql
             HE.noParams
             dec
             True



{-Encoder for type News-}
someNewsEncoder :: HE.Params News
someNewsEncoder = 
          (name >$< HE.param (HE.nonNullable HE.text)) <>
          (category >$< HE.param (HE.nullable HE.text)) <>
          (tags >$< HE.param (HE.nullable $ HE.array $ HE.dimension DF.foldl' $ HE.element $ HE.nonNullable HE.text)) <>
          (text_of_new >$< HE.param (HE.nullable HE.text)) <>
          (fromIntegral . autor_id >$< HE.param (HE.nonNullable HE.int8)) <>
          (fromIntegral . id_of_new >$< HE.param (HE.nonNullable HE.int8)) <> 
          (date_of_create_new >$< HE.param (HE.nullable HE.date))          

{-Decoder-}
someNewsDecoder :: HD.Result (Vector News)
someNewsDecoder =  HD.rowVector $ Types.News <$> 
          (HD.column (HD.nonNullable HD.text)) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (HD.column $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nonNullable HD.text))) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nullable HD.date))

{-Encoder for type Autors-}
someAutorsEncoder :: HE.Params Autors
someAutorsEncoder =  
          (fromIntegral . user_id_autors >$< HE.param (HE.nonNullable HE.int8)) <>
          (description >$< HE.param (HE.nullable HE.text)) <>
          (news_id >$< HE.param (HE.nullable $ HE.array $ HE.dimension DF.foldl' $ HE.element $ HE.nonNullable $ contramap fromIntegral HE.int8))

{-Decoder-}
someAutorsDecoder :: HD.Result (Vector Autors)
someAutorsDecoder = HD.rowVector $ Types.Autors <$>
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
someUsersDecoder :: HD.Result (Vector Users)
someUsersDecoder = HD.rowVector $ Types.Users <$>
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
someCommentsDecoder :: HD.Result (Vector Comments)
someCommentsDecoder = HD.rowVector $ Types.Comments <$>
          (HD.column (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) 
          
{-Encoder for type Draft-}
someDraftsEncoder :: HE.Params Draft                      
someDraftsEncoder = 
          (fromIntegral . id_of_draft >$< HE.param (HE.nonNullable HE.int8)) <>
          (fromIntegral . id_of_user  >$< HE.param (HE.nonNullable HE.int8)) <>
          (text_of_draft >$< HE.param (HE.nonNullable HE.text))
          


{-Decoder-}
someDraftsDecoder :: HD.Result (Vector Draft)
someDraftsDecoder = HD.rowVector $ Types.Draft <$>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nonNullable HD.text)) 



connectToDB2 :: a -> BS.ByteString -> HE.Params a -> IO ()
connectToDB2 data1 query enc =
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
        queryResult <- HS.run (selectTasksSession2 data1 query enc) connection
        HC.release connection
        case queryResult of
          Right result -> return result
          Left err -> error $ show err


selectTasksSession2 :: a ->  BS.ByteString -> HE.Params a -> HS.Session ()
selectTasksSession2  data1 query enc = HS.statement data1 (selectTasksStatement2 query enc) 


selectTasksStatement2 ::  BS.ByteString ->  HE.Params a -> HST.Statement a ()
selectTasksStatement2 sql enc =
             HST.Statement 
             sql
             enc
             HD.noResult
             True

            