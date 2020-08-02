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

connectToDB :: BS.ByteString -> IO [News]
connectToDB query =
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
        queryResult <- HS.run (selectTasksSession query) connection
        HC.release connection
        case queryResult of
          Right result -> return result
          Left err -> error $ show err


selectTasksSession :: BS.ByteString -> HS.Session [News]
selectTasksSession query = HS.statement () $ selectTasksStatement query

selectTasksStatement :: BS.ByteString -> HST.Statement () [News]
selectTasksStatement query =
  HST.Statement
    query
    HE.noParams
    someNewsDecoder
    True

someNewsEncoder :: HE.Params News
someNewsEncoder = 
  (name >$< HE.param (HE.nonNullable HE.text)) <>
  (data_of_create >$< HE.param (HE.nullable HE.text)) <>
  (fromIntegral . autor_id >$< HE.param (HE.nonNullable HE.int8)) <>
  (category >$< HE.param (HE.nullable HE.text)) <>
  (tags >$< HE.param (HE.nullable $ HE.array $ HE.dimension DF.foldl' $ HE.element $ HE.nonNullable HE.text)) <>
  (text_of_new >$< HE.param (HE.nullable HE.text)) <>
  (fromIntegral . id_of_new >$< HE.param (HE.nonNullable HE.int8)) 

someNewsDecoder :: HD.Result [News]
someNewsDecoder =  HD.rowList $ News <$> 
                            (HD.column (HD.nonNullable HD.text)) <*>
                            (HD.column (HD.nullable HD.text)) <*>
                            (fromIntegral <$> (HD.column (HD.nonNullable HD.int8))) <*>
                            (HD.column (HD.nullable HD.text)) <*>
                            (HD.column $ HD.nullable $ HD.array $ HD.dimension Control.Monad.replicateM (HD.element (HD.nonNullable HD.text))) <*>
                            (HD.column (HD.nullable HD.text)) <*>
                            (fromIntegral <$> (HD.column (HD.nonNullable HD.int8)))