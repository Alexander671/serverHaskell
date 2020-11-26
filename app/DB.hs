{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module DB where
import Types
import Logging.Logging as L ( runLog )
import Logging.Level (LogLevel(DEBUG, INFO) )
{------------------------------------}
import qualified Hasql.Connection as HC 
import qualified Hasql.Session as HS
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Statement as HST
{------------------------------------}
import Data.Vector ( Vector )
import Network.URI.Encode (decodeBSToText,encodeTextToBS)
{------------------------------------}
import qualified Data.ByteString.Char8    as BS
{------------------------------------}
import qualified Control.Monad
{------------------------------------}
import Data.Functor.Contravariant
    ( Contravariant(contramap), (>$<) ) 
import Data.Aeson (ToJSON, encode)
import qualified Data.Vector as DV
import qualified Data.Text as DT

{------------------------------------}

connectToDB :: ToJSON a => String -> BS.ByteString -> HD.Result (Vector a) -> IO (Status a)
connectToDB role query dec  =
  let
    connectionSettings :: HC.Settings
    connectionSettings =
      HC.settings
        "localhost"
        5432
        "postgres"
        "password"
        "postgres23"
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
                        runLog INFO $ show $ encode $ Status {ok=True,result=Just result1,error_description=Nothing,error_id=Nothing,role=role}
                        return $ Status {ok=True,result=Just $ result1,error_description=Nothing,error_id=Nothing,role=role} -- понять какой тип использовать для ошибки
          Left err -> do
                     runLog DEBUG $ show $ err
                     return $ Status {ok=False,result=Nothing,error_description=Just "problem with syntax",error_id=Just 404,role=role}


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
someNewsEncoder :: HE.Params NewsEncoder
someNewsEncoder = 
          (name_en >$< HE.param (HE.nonNullable HE.text)) <>
          (fromIntegral . category_en >$< HE.param (HE.nonNullable HE.int8)) <> 
          (tags_en >$< someTagsEncoder) <>
          (text_of_new_en >$< HE.param (HE.nullable HE.text)) <>
          (fromIntegral . id_of_new_en >$< HE.param (HE.nonNullable HE.int8)) <> 
          (fromIntegral . autor_id_en >$< HE.param (HE.nonNullable HE.int8)) <> 
          (date_of_create_new_en >$< HE.param (HE.nullable HE.date))          
         
{-Decoder-}
someNewsDecoder :: HD.Result (Vector News)
someNewsDecoder =  HD.rowVector $ Types.News <$> 
          (HD.column $ HD.nullable HD.text) <*>
          (HD.column $ HD.nullable someCategoriesDecoder) <*>
          (HD.column $ HD.nullable someTagsDecoder) <*>
          (HD.column $ HD.nullable HD.text) <*>
          (HD.column (HD.nonNullable $ fromIntegral <$> HD.int8)) <*>
          (HD.column $ HD.nullable someAutorsDecoder) <*>
          (HD.column $ HD.nullable someImageDecoder) <*>
          (HD.column $ HD.nullable HD.date) <*>
          (HD.column $ HD.nullable someCommentsDecoder) <*>
          (HD.column $ HD.nullable HD.text)

{-Decoder-}
someNewsDecoderNotNested :: HD.Result (Vector NewsNotNested)
someNewsDecoderNotNested =  HD.rowVector $ Types.NewsNotNested <$> 
          (HD.column $ HD.nullable HD.text) <*>
          (HD.column $ HD.nullable HD.text) <*>
          (HD.column (HD.nullable $ fromIntegral <$> HD.int8)) <*>
          (HD.column (HD.nullable $ fromIntegral <$> HD.int8)) <*>
          (HD.column $ HD.nullable HD.date) <*>
          (HD.column (HD.nullable $ fromIntegral <$> HD.int8)) <*>
          (HD.column $ HD.nullable (HD.vectorArray (HD.nonNullable (fromIntegral <$> HD.int8)))) <*>
          (HD.column $ HD.nullable HD.text)
          

{-Encoder for type Autors-}
someAutorsEncoder :: HE.Params AutorsEncoder
someAutorsEncoder =  
          (description_en >$< HE.param (HE.nullable HE.text)) <>
          (user_id_autors_en >$< HE.param (HE.nonNullable $ contramap fromIntegral HE.int8)) 

{-Decoder-}
someAutorsDecoder :: HD.Value  Autors
someAutorsDecoder = HD.composite $ Types.Autors <$>
          (HD.field (HD.nullable HD.text)) <*>
          (HD.field $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nullable (fromIntegral <$> HD.int8)))) <*>
          (HD.field (HD.nonNullable someUsersDecoder))

{-DecoderNotNested-}
someAutorsDecoderNotNested :: HD.Result (Vector Autors)
someAutorsDecoderNotNested = HD.rowVector $ Types.Autors <$>
          (HD.column (HD.nullable HD.text)) <*>
          (HD.column $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nullable (fromIntegral <$> HD.int8)))) <*>
          (HD.column (HD.nonNullable someUsersDecoder))



{-Encoder for type Category-}
someTagsEncoder :: HE.Params (Vector Tags)
someTagsEncoder = 
        (HE.param $ HE.nonNullable $ fmap tag_name >$< HE.foldableArray (HE.nonNullable HE.text)) <>
        (HE.param $ HE.nonNullable $ (fmap tag_id) >$< HE.foldableArray (HE.nonNullable $ contramap fromIntegral HE.int8)) 
          
{-Decoder-}
someTagsDecoder :: HD.Value (Vector (Maybe Tags))
someTagsDecoder = HD.vectorArray $ HD.nullable $ HD.composite $ Types.Tags <$>
          (HD.field (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) 

{-Decoder for NotNested-}
someTagsDecoderNotNested :: HD.Result (Vector Tags)
someTagsDecoderNotNested = HD.rowVector $ Types.Tags <$>
          (HD.column (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) 

{-Decoder for NotNested-}
someTagsEncoderNotNested :: HE.Params (Tags)
someTagsEncoderNotNested = 
        (tag_name >$< HE.param (HE.nonNullable HE.text)) <>
        (tag_id >$< HE.param (HE.nonNullable $ contramap fromIntegral HE.int8)) 

{-Encoder for type Category-}
someImageEncoder :: HE.Params (Vector Images)
someImageEncoder = 
        (HE.param $ HE.nonNullable $ fmap id_of_image >$< HE.foldableArray (HE.nonNullable HE.text)) <>
        (HE.param $ HE.nonNullable $ (fmap image_new) >$< HE.foldableArray (HE.nonNullable $ contramap fromIntegral HE.int8)) 
          
{-Decoder-}
someImageDecoder :: HD.Value (Vector (Maybe Images))
someImageDecoder = HD.vectorArray $ HD.nullable $ HD.composite $ Types.Images <$>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) <*>
          (HD.field (HD.nonNullable HD.text)) 


{-Encoder for type Users-}
someUsersEncoder :: HE.Params (Users)
someUsersEncoder = 
          (image >$< HE.param (HE.nullable $ contramap encodeTextToBS HE.bytea)) <>
          (date_of_create_user >$< HE.param (HE.nonNullable HE.date)) <>
          (user_id >$< HE.param (HE.nonNullable $ contramap fromIntegral HE.int8)) <>
          (first_name >$< HE.param (HE.nullable HE.text)) <>
          (second_name >$< HE.param (HE.nullable HE.text)) 

{-Decoder-}
someUsersDecoder :: HD.Value (Users)
someUsersDecoder =  HD.composite $ Types.Users <$>
          (HD.field (HD.nonNullable HD.date)) <*>
          (HD.field (HD.nullable HD.text)) <*>
          (HD.field (HD.nullable HD.text)) <*>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) <*>
          (HD.field (HD.nullable $ decodeBSToText <$> HD.bytea))

{-Decoder for NotNested-}
someUsersDecoderNotNested :: HD.Result (Vector Users)
someUsersDecoderNotNested =  HD.rowVector $ Types.Users <$>
          (HD.column (HD.nonNullable HD.date)) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (HD.column(HD.nullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nullable $ decodeBSToText <$> HD.bytea)) 
          
{-Encoder for type Comment-}
someCommentsEncoder :: HE.Params Comments                      
someCommentsEncoder = 
          (fromIntegral . id_of_comment >$< HE.param (HE.nonNullable HE.int8)) <>
          (user_id_comment >$< someUsersEncoder) <>
          (fromIntegral . id_of_new_comment >$< HE.param (HE.nonNullable HE.int8)) <>
          (text_of_comment >$< HE.param (HE.nonNullable HE.text)) 

{-Encoder for type Comment-}
someCommentsEncoderNotNested :: HE.Params CommentsNotNested                      
someCommentsEncoderNotNested = 
          (fromIntegral . id_of_comment_nt >$< HE.param (HE.nonNullable HE.int8)) <>
          (user_id_comment_nt >$< HE.param (HE.nonNullable $ contramap fromIntegral HE.int8)) <>
          (fromIntegral . id_of_new_comment_nt >$< HE.param (HE.nonNullable HE.int8)) <>
          (text_of_comment_nt >$< HE.param (HE.nonNullable HE.text))        
{-Decoder-}
someCommentsDecoder :: HD.Value (Vector (Maybe Comments))
someCommentsDecoder = HD.vectorArray $ HD.nullable $ HD.composite $ Types.Comments <$>
          (HD.field (HD.nonNullable someUsersDecoder)) <*>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.field (HD.nonNullable HD.int8))) <*>
          (HD.field (HD.nonNullable HD.text)) 


{-Decoder NotNested-}
someCommentsDecoderNotNested :: HD.Result (Vector CommentsNotNested)
someCommentsDecoderNotNested = HD.rowVector $ Types.CommentsNotNested <$>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*> 
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nonNullable HD.text)) 
          
{-Encoder for type Draft-}
someDraftsEncoder :: HE.Params Draft                      
someDraftsEncoder = 
          (fromIntegral . id_of_user  >$< HE.param (HE.nonNullable HE.int8)) <>
          (text_of_draft >$< HE.param (HE.nullable HE.text)) <>
          (fromIntegral . id_of_draft >$< HE.param (HE.nonNullable HE.int8)) <>
          (fromIntegral . category_id_draft  >$< HE.param (HE.nonNullable HE.int8)) <>
          (photo_draft >$< HE.param (HE.nullable HE.text)) 
          
          
{-Decoder-}
someDraftsDecoder :: HD.Result (Vector Draft)
someDraftsDecoder = HD.rowVector $ Types.Draft <$>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
          (HD.column (HD.nullable (fmap (DT.pack . BS.unpack) HD.bytea)))         
          

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

{-Decoder NotNested-}
someCategoriesDecoderNotNested :: HD.Result (Vector Category)
someCategoriesDecoderNotNested = HD.rowVector $ Types.Category <$>
          (fmap fromIntegral <$> (HD.column (HD.nullable HD.int8))) <*>
          (HD.column (HD.nonNullable HD.text)) <*>
          (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) 

{-Decoder Registration-}
someLogInUpEncoder ::  HE.Params LogInUp
someLogInUpEncoder = 
          (fromIntegral <$> user_id_reg >$< HE.param (HE.nonNullable HE.int8)) <>
          (login  >$< HE.param (HE.nonNullable HE.text)) <>
          (password  >$< HE.param (HE.nonNullable HE.text)) <>
          (token >$< HE.param (HE.nonNullable HE.text)) 
          
connectToDB2 :: a -> BS.ByteString -> HE.Params a -> IO (StatusInsert a)
connectToDB2 data1 query enc =
  let
    connectionSettings :: HC.Settings
    connectionSettings =
      HC.settings
        "localhost"
        (fromInteger 5432)
        "postgres"
        "password"
        "postgres23"
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
          Right result1 -> do
                        putStrLn $ show $ encode (StatusInsert {ok1=True,result1=Just result1,error_description1=Nothing,error_id1=Nothing} :: Types.StatusInsert News)
                        return $ StatusInsert {ok1=True,result1=Just result1,error_description1=Nothing,error_id1=Nothing} -- понять какой тип использовать для ошибки
          Left err -> do
                     putStrLn $ show $ encode (StatusInsert {ok1=False,result1=Nothing,error_description1=Just $ show err,error_id1=Just 404} :: Types.StatusInsert News)
                     return $ StatusInsert {ok1=False,result1=Nothing,error_description1=Just "problem with syntax",error_id1=Just 404}



selectTasksSession2 :: a ->  BS.ByteString -> HE.Params a -> HS.Session ()
selectTasksSession2  data1 query enc = HS.statement data1 (selectTasksStatement2 query enc) 


selectTasksStatement2 ::  BS.ByteString ->  HE.Params a -> HST.Statement a ()
selectTasksStatement2 sql enc =
             HST.Statement 
             sql
             enc
             HD.noResult
             True

            