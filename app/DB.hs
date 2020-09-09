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
import Data.Vector ( Vector )
import Data.Text 
import Network.URI.Encode (decodeBSToText,encodeTextToBS)
{------------------------------------}
import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
{------------------------------------}
import qualified Control.Monad
import Data.Foldable as DF ( Foldable(foldl') )
{------------------------------------}
import Data.Functor.Contravariant
    ( Contravariant(contramap), (>$<) ) 
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
                        putStrLn $ show $ encode $ Status {ok=True,result=Just result1,error_description=Nothing,error_id=Nothing}
                        return $ Status {ok=True,result=Just $ result1,error_description=Nothing,error_id=Nothing} -- понять какой тип использовать для ошибки
          Left err -> do
                     putStrLn $ show $ encode (Status {ok=False,result=Nothing,error_description=Just $ show err,error_id=Just 404} :: Types.Status News)
                     return $ Status {ok=False,result=Nothing,error_description=Just "problem with syntax",error_id=Just 404}


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
          (category >$< (someCategoriesEncoder)) <>
          (tags >$< someTagsEncoder) <>
          (text_of_new >$< HE.param (HE.nullable HE.text)) <>
          (fromIntegral . id_of_new >$< HE.param (HE.nonNullable HE.int8)) <> 
          (autor_id >$< (someAutorsEncoder)) <>
          (date_of_create_new >$< HE.param (HE.nullable HE.date))          

{-Decoder-}
someNewsDecoder :: HD.Result (Vector News)
someNewsDecoder =  HD.rowVector $ Types.News <$> 
          (HD.column $ HD.nonNullable HD.text) <*>
          (HD.column $ HD.nonNullable someCategoriesDecoder) <*>
          (HD.column $ HD.nonNullable someTagsDecoder) <*>
          (HD.column $ HD.nullable HD.text) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column $ HD.nonNullable someAutorsDecoder) <*>
          (HD.column $ HD.nullable HD.date)

{-Encoder for type Autors-}
someAutorsEncoder :: HE.Params Autors
someAutorsEncoder =  
          (fromIntegral . user_id_autors >$< HE.param (HE.nonNullable HE.int8)) <>
          (description >$< HE.param (HE.nullable HE.text)) <>
          (news_id >$< HE.param (HE.nullable $ HE.array $ HE.dimension DF.foldl' $ HE.element $ HE.nonNullable $ contramap fromIntegral HE.int8))

{-Decoder-}

someAutorsDecoder :: HD.Value Autors
someAutorsDecoder = HD.composite $ Types.Autors <$>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) <*>
          (HD.field (HD.nullable HD.text)) <*>
          (HD.field $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nonNullable (fromIntegral <$> HD.int8)))) 
          
{-Encoder for type Users-}
someUsersEncoder :: HE.Params Users
someUsersEncoder = 
          (image >$< HE.param (HE.nullable $ contramap encodeTextToBS HE.bytea)) <>
          (date_of_create_user >$< HE.param (HE.nonNullable HE.date)) <>
          (fromIntegral . user_id >$< HE.param (HE.nonNullable HE.int8)) <>
          (first_name >$< HE.param (HE.nonNullable HE.text)) <>
          (second_name >$< HE.param (HE.nonNullable HE.text)) 

{-Decoder-}
someUsersDecoder :: HD.Result (Vector Users)
someUsersDecoder = HD.rowVector $ Types.Users <$>
          (HD.column (HD.nullable $ decodeBSToText <$> HD.bytea)) <*>
          (HD.column (HD.nonNullable HD.date)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nonNullable HD.text)) <*>
          (HD.column (HD.nonNullable HD.text))
          
{-Encoder for type Comment-}
someCommentsEncoder :: HE.Params Comments                      
someCommentsEncoder = 
          (fromIntegral . id_of_comment >$< HE.param (HE.nonNullable HE.int8)) <>
          (fromIntegral . user_id_comment >$< HE.param (HE.nonNullable HE.int8)) <>
          (fromIntegral . id_of_new_comment >$< HE.param (HE.nonNullable HE.int8)) <>
          (text_of_comment >$< HE.param (HE.nonNullable HE.text)) 
          

{-Decoder-}
someCommentsDecoder :: HD.Result (Vector Comments)
someCommentsDecoder = HD.rowVector $ Types.Comments <$>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*> 
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nonNullable HD.text)) 
          
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

{-Encoder for type Category-}
someCategoriesEncoder :: HE.Params Category                      
someCategoriesEncoder = 
          (fmap fromIntegral <$> parent_id >$< HE.param (HE.nullable HE.int8)) <>
          (category_name  >$< HE.param (HE.nonNullable HE.text)) <>
          (fromIntegral <$> category_id  >$< HE.param (HE.nonNullable HE.int8))

{-Decoder-}
someCategoriesDecoder :: HD.Value Category
someCategoriesDecoder = HD.composite $ Types.Category <$>
          (fmap fromIntegral <$> (HD.field (HD.nullable HD.int8))) <*>
          (HD.field (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) 

{-Encoder for type Category-}
someTagsEncoder :: HE.Params Tags                      
someTagsEncoder = 
          (fromIntegral <$> tag_id >$< HE.param (HE.nonNullable HE.int8)) <>
          (tag_name  >$< HE.param (HE.nonNullable HE.text))

{-Decoder-}
someTagsDecoder :: HD.Value Tags
someTagsDecoder = HD.composite $ Types.Tags <$>
          (HD.field (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) 
          



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

            