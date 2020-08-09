{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
{----------------------------------------}
import Lib
import DB
--import JWT
import qualified Hasql.Decoders as HD (Result)
import qualified Hasql.Encoders as HE (Params)
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
import           Network.URI.Encode (decodeBSToText,encodeTextToBS)      
{----------------------------------------}
import           Network.HTTP.Types       (Status, badRequest400, hContentType,
                                           methodGet, methodDelete, methodPut, methodPost, notFound404, status200,
                                           statusCode)
import           Network.Wai              (Application, ResponseReceived, Request, Middleware, Response,
                                           queryString, pathInfo,
                                           rawQueryString, requestMethod,
                                           responseLBS, responseStatus)
import           Network.Wai.Handler.Warp (run)
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

yousecret = "SERVER_HASKELL" -- for jwt

-- Main function

main :: IO ()
main = do
  putStrLn "Serving (hit Ctrl+C to stop)..."
  run 8000 (application)

-- Web app
-- Parse of token and taking user role
application :: Application
application req respond =              
      case (fmap (Prelude.lookup "userRole") $ jwtCheck route) of
       Just (Just role) -> application2 (DA.encode role) req respond
       Nothing          -> respond $ responseBadRequestJSON $ DA.encode ("error: 2"::Text)
      ----------------------------
      where route = Prelude.head $ helphead (pathInfo req)
            jwtCheck content = fmap (toList . unClaimsMap . unregisteredClaims . JWT.claims) (JWT.decodeAndVerifySignature (HMACSecret yousecret) content)
            helphead [] = ["error: empty list"]
            helphead (x:xs) = [x]


application2 :: ByteString -> Application
application2 role req respond = 
  case pathInfo req of
    (_:"news":_)     -> routeNews     role req respond   
    (_:"autors":_)   -> routeAutors   role req respond
    (_:"users":_)    -> routeUsers    role req respond
    (_:"comments":_) -> routeComments role req respond
    _              -> respond $ responseOkJSON $ encode ("error"::Text)  

routeNews :: ByteString -> Application
routeNews role req respond = 
    case pathInfo req of
    [_,"news"] -> dbQuery someNewsDecoder someNewsEncoder "SELECT * FROM news;" req respond
    [_,"news","filter"] ->
        case queryString req of 
        ("date_of_create_at_gt",Just x):xs -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news WHERE date_of_create > '" ++ show x ++ "';") req respond
        ("date_of_create_at_lt",Just x):xs -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news WHERE date_of_create < '" ++ show x ++ "';") req respond
        ("name_of_autor", Just x):xs       -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  AND u.first_name = '" ++ show x ++ "';") req respond
        ("category", Just x):xs            -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news WHERE category = '" ++ BS.unpack x ++ "';") req respond
        -- name_of_autor, изменить структуру бд, а также типы в Types, сделать поле user... ::Users 
        _  -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
    [_,"news","order"] ->
        case queryString req of
        [] -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
        ("date_of_create",Just x):xs  -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news ORDER BY date_of_create " ++ show x ++ "';") req respond
        ("category",Just x):xs        -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news ORDER BY category " ++ BS.unpack x ++ ";") req respond
        ("name_of_autor", Just x):xs  -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  ORDER BY u.first_name " ++ BS.unpack x ++ ";") req respond
        _ -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
        {------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ encode ("error" ::Text)

routeAutors ::  ByteString -> Application
routeAutors role req respond  
  | role == "\"Admin\"" =
   case requestMethod req of
      "GET"  ->  case pathInfo req of  
                     [_,"autors"] -> dbQuery someAutorsDecoder someAutorsEncoder "SELECT * FROM autors;" req respond
                     _            -> respond $ responseNotFoundJSON $ encode ("error: case only autors" ::Text)
      "POST" -> case lookupStuff ["user_id","description","news_id"] req of 
                    ([Just(Just x),Just (Just y),Just (Just z)]) -> dbQuery someAutorsDecoder someAutorsEncoder "SELECT * FROM autors;" req respond
                    _ ->   respond $ responseOkJSON $ encode ("Problem with queryString (autors)" ::Text)
                    where  lookupStuff []        lst = []
                           lookupStuff (stuff:s) lst = (Prelude.lookup stuff $ queryString lst) : (lookupStuff s lst)
      
      _ ->  respond $ responseNotFoundJSON $ encode ("Problem with rqstMethod (autors)" ::Text)
    {------------------------------------------------------------}
  | otherwise = respond $ responseNotFoundJSON role

routeUsers :: ByteString ->  Application
routeUsers role req respond = do
    res <- connectToDB "SELECT * FROM users;"  someUsersDecoder someUsersEncoder
    (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("Not available path"::Text))) 

routeComments ::  ByteString ->  Application
routeComments role req respond = do
    res <- connectToDB "SELECT * FROM comments;" someCommentsDecoder someCommentsEncoder
    (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("Not available path"::Text))) 

dbQuery :: ToJSON a => HD.Result [a] -> HE.Params a -> BS.ByteString ->  Application
dbQuery dec enc sql req respond = do 
  res <- connectToDB sql dec enc
  (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("error12"::Text)))               

{-dbQueryPut :: ToJSON a => HE.Params a -> BS.ByteString ->  Application
dbQueryPut enc sql req respond = do
  res <- connectToDB "SELECT * FROM news;" enc
  (respond $ responseOkJSON
              (case rawQueryString req of
                "" -> encode res
                _  -> encode ("error12"::Text))) 
-}
responseOkJSON,responseNotFoundJSON,responseBadRequestJSON :: ByteString -> Response
responseOkJSON = responsePlainTextJSON status200
responseNotFoundJSON = responsePlainTextJSON notFound404
responseBadRequestJSON = responsePlainTextJSON badRequest400

responsePlainTextJSON,responsePlainText  :: Status -> ByteString -> Response
responsePlainTextJSON = (`responseLBS` [(hContentType,  "application/json; charset=utf-8")])
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])