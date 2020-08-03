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
import Network.URI.Encode (decodeBSToText,encodeTextToBS)      
{----------------------------------------}
import           Network.HTTP.Types       (Status, badRequest400, hContentType,
                                           methodGet, methodDelete, methodPut, notFound404, status200,
                                           statusCode)
 
import           Network.Wai              (Application, ResponseReceived, Request, Middleware, Response,
                                           queryString, pathInfo,
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
    case pathInfo req of
     ["news"]     -> case queryString req of
                    [] -> routeNews "SELECT * FROM news;" req respond
                    [("category", Nothing)]  -> routeNews "SELECT * FROM news ORDER BY category ASC;" req respond
                    [("date_of_create_at",Just x)] -> routeNews (BS.pack $ "SELECT * FROM news WHERE date_of_create= '" ++ show x ++ "';") req respond
                    _  -> respond $ responseNotFoundJSON $ encode ("error" ::Text)

     ["autors"]   -> routeAutors   req respond
     ["users"]    -> routeUsers    req respond
     ["comments"] -> routeComments req respond
     _            -> respond $ responseOkJSON $ encode ("error"::Text)
  | otherwise =
    respond
    $ responseBadRequestJSON "Only GET method is allowed!"

-- Get method (news,autors,users)  
routeNews :: BS.ByteString ->  Application
routeNews sql req respond = do
    res <- connectToDB sql someNewsDecoder
    (respond $ responseOkJSON $ encode res)
              

routeAutors :: Application
routeAutors req respond = do
    res <- connectToDB "SELECT * FROM autors;" someAutorsDecoder
    (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("error"::Text))) 

routeUsers :: Application
routeUsers req respond = do
    res <- connectToDB "SELECT * FROM users;" someUsersDecoder
    (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("Not available path"::Text))) 

routeComments :: Application
routeComments req respond = do
    res <- connectToDB "SELECT * FROM comments;" someCommentsDecoder
    (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("Not available path"::Text))) 


responseOkJSON,responseNotFoundJSON,responseBadRequestJSON :: ByteString -> Response
responseOkJSON = responsePlainTextJSON status200
responseNotFoundJSON = responsePlainTextJSON notFound404
responseBadRequestJSON = responsePlainTextJSON badRequest400

responsePlainTextJSON,responsePlainText  :: Status -> ByteString -> Response
responsePlainTextJSON = (`responseLBS` [(hContentType,  "application/json; charset=utf-8")])
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])