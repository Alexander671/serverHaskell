{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
{----------------------------------------}
import Lib
import DB
{----------------------------------------}
import Prelude                  hiding (lookup)
{----------------------------------------}
import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
{----------------------------------------}
import           Data.Map.Strict          (Map, fromList, keys, lookup)
import           Data.Text                as DT       
{----------------------------------------}
import           Network.HTTP.Types       (Status, badRequest400, hContentType,
                                           methodGet, methodDelete, methodPut, notFound404, status200,
                                           statusCode)
 
import           Network.Wai              (Application, ResponseReceived, Request, Middleware, Response,
                                           queryString, rawPathInfo,
                                           rawQueryString, requestMethod,
                                           responseLBS, responseStatus)
import Network.Wai.Handler.Warp (run)
{----------------------------------------}
import Data.Aeson


type PathName        = BS.ByteString
type PathDescription = BS.ByteString
type PathArg         = BS.ByteString
type PathResult      = BS.ByteString
type PathSpec        = ( PathDescription
                           , (PathArg -> PathResult) )


-- Main function

main :: IO ()
main = do
  putStrLn "Serving (hit Ctrl+C to stop)..."
  run 8000 (application)

-- web app
application :: Application
application req respond
  | requestMethod req == methodGet =
    manageGet req respond

  | otherwise =
    respond
    $ responseBadRequest "Only GET method is allowed!"
  
manageGet :: Application
manageGet req respond = do
    res <- connectToDB "SELECT * FROM news"
    (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> "error")) 

-- Misc functions

responseOk, responseNotFound, responseBadRequest :: ByteString -> Response
responseOk         = responsePlainText status200
responseNotFound   = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responseOkJSON :: ByteString -> Response
responseOkJSON = responsePlainTextJSON status200

responsePlainTextJSON :: Status -> ByteString -> Response
responsePlainTextJSON = (`responseLBS` [(hContentType,  "application/json; charset=utf-8")])

responsePlainText :: Status -> ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])