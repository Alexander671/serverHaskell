{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
{----------------------------------------}
import Lib
import DB
--import JWT
import Web.JWT as JWT (ClaimsMap, unregisteredClaims, 
                       claims, header, unClaimsMap, signature,
                       Signer(HMACSecret), decode,decodeAndVerifySignature) 
{----------------------------------------}
import Prelude
{----------------------------------------}
import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Lazy     (ByteString)
import qualified Data.ByteString.Lazy     as LBS
{----------------------------------------}
import           Data.Map.Strict       (toList, Map, fromList, keys, lookup) 
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
import Data.Aeson as DA
import Data.Time (Day)
import           GHC.Generics  



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
application req respond =              
      case (fmap (Prelude.lookup "userRole") $ jwtcheck route) of
       Just (Just role) -> application2 role req respond
       Just (Nothing)   -> respond $ responseBadRequestJSON "error: 1"
       Nothing          -> respond $ responseBadRequestJSON "error: 2"
      ----------------------------
      where route = unpack $ Prelude.head $ head1 (pathInfo req)
            jwtcheck content = fmap (toList . unClaimsMap . unregisteredClaims . JWT.claims) (JWT.decodeAndVerifySignature (HMACSecret "your secret") $ DT.pack content)

head1 [] = ["error: empty list"]
head1 (x:xs) = [x]


application2 :: Value -> Application
application2 role req respond
  | requestMethod req == methodGet =
  case pathInfo req of
    (_:"news":_)     -> routeNews     req respond   
    (_:"autors":_)   -> routeAutors   req respond
    (_:"users":_)    -> routeUsers    req respond
    (_:"comments":_) -> routeComments req respond
    _              -> respond $ responseOkJSON $ encode ("error"::Text)  
  | otherwise =
    respond $ responseBadRequestJSON "Only GET method is allowed!"

routeNews :: Application
routeNews req respond = 
    case pathInfo req of
    [_,"news"] -> dbNews "SELECT * FROM news;" req respond
    [_,"news","filter"] ->
        case queryString req of 
        _:("date_of_create_at_gt",Just x):xs -> dbNews (BS.pack $ "SELECT * FROM news WHERE date_of_create > '" ++ show x ++ "';") req respond
        _:("date_of_create_at_lt",Just x):xs -> dbNews (BS.pack $ "SELECT * FROM news WHERE date_of_create < '" ++ show x ++ "';") req respond
        _:("name_of_autor", Just x):xs       -> dbNews (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  AND u.first_name = '" ++ show x ++ "';") req respond
        _:("category", Just x):xs            -> dbNews (BS.pack $ "SELECT * FROM news WHERE category = '" ++ BS.unpack x ++ "';") req respond
        -- name_of_autor, изменить структуру бд, а также типы в Types, сделать поле user... ::Users 
        _  -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
    [_,"news","order"] ->
        case queryString req of
        [] -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
        _:("date_of_create",Just x):xs  -> dbNews (BS.pack $ "SELECT * FROM news ORDER BY date_of_create " ++ show x ++ "';") req respond
        _:("category",Just x):xs        -> dbNews (BS.pack $ "SELECT * FROM news ORDER BY category " ++ BS.unpack x ++ ";") req respond
        _:("name_of_autor", Just x):xs  -> dbNews (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  ORDER BY u.first_name " ++ BS.unpack x ++ ";") req respond
        _ -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
        {------------------------------------------------------------}

dbNews :: BS.ByteString ->  Application
dbNews sql req respond = do 
  res <- connectToDB sql someAutorsDecoder
  (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("error"::Text))) 
              

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