{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
{----------------------------------------}
import DB
import Types
--import JWT
import qualified Hasql.Decoders as HD (rowVector, rowList, noResult, Result,Row)
import qualified Hasql.Encoders as HE (noParams, Params)
{----------------------------------------}
import Web.JWT as JWT (ClaimsMap, unregisteredClaims, 
                       claims, header, unClaimsMap, signature,
                       Signer(HMACSecret), decode,decodeAndVerifySignature) 
{----------------------------------------}
import qualified Data.ByteString.Char8    as BS 
import           Data.ByteString.Lazy     (toStrict, fromStrict, ByteString)
import           Data.ByteString.Lazy.UTF8
import           Data.Map.Strict          (toList, Map, keys, lookup) 
import           Data.Text                as DT 
import           Network.URI.Encode       (decodeBSToText,encodeTextToBS)      
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
import Data.Vector as DV (Vector) 
import Control.Monad (join)
{----------------------------------------}

yousecret = "SERVER_HASKELL" -- for jwt

-- Main function

main :: IO ()
main = do
  putStrLn "Serving (hit Ctrl+C to stop)..."
  run 8000 (application)

-- Web app
-- Parse of jwt-token and taking user role
application :: Application
application req respond =              
      case (fmap (Prelude.lookup "userRole") $ jwtCheck (route req)) of
       Just (Just role) -> application2 (DA.encode role) req respond
       Nothing          -> respond $ responseBadRequestJSON $ DA.encode ("error: 2"::Text)
      ----------------------------

--help function for application
route :: Request -> Text
route req = Prelude.head $ helphead (pathInfo req)
jwtCheck :: Text -> Maybe [(Text, Value)]
jwtCheck content = fmap (toList . unClaimsMap . unregisteredClaims . JWT.claims) (JWT.decodeAndVerifySignature (HMACSecret yousecret) content)
helphead [] = ["error: empty list"]
helphead (x:xs) = [x]


application2 :: ByteString -> Application
application2 role req respond = 
  case pathInfo req of
    (_:"news":_)             -> routeNews      role req respond   
    (_:"autors":_)           -> routeAutors    role req respond
    (_:"drafts":_)           -> routeDrafts    role req respond
    --(_:"comments":_) -> routeComments role req respond
    --(_:"users":_)    -> routeUsers    role req respond
    --(_:"tags":_)     -> routeTags     role req respond
    --(_:"category":_) -> routeCategory role req respond
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
{-
--*!      category_id
--*!      tag_id
--*!      in,all,any
-}      
        _  -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
    {----------------------------------------------------------------}
    [_,"news","order"] ->
        case queryString req of
        [] -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
        ("date_of_create",Just x):xs  -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news ORDER BY date_of_create " ++ show x ++ "';") req respond
        ("category",Just x):xs        -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news ORDER BY category " ++ BS.unpack x ++ ";") req respond
        ("name_of_autor", Just x):xs  -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  ORDER BY u.first_name " ++ BS.unpack x ++ ";") req respond
{-
--*!      amount of photo
-}
{-
--*! search string (text_of_new,first_name,tag,category)
-}    
        _ -> respond $ responseNotFoundJSON $ encode ("error" ::Text)
    {------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ encode ("error" ::Text)

routeAutors ::  ByteString -> Application
routeAutors role req respond  
  | role == "\"Admin\"" =
       case requestMethod req of
         "GET"  ->  case pathInfo req of  
                     [_,"autors"] -> dbQuery someAutorsDecoder someAutorsEncoder "SELECT * FROM autors;" req respond
                     _            -> respond $ responseNotFoundJSON $ encode ("Problem wiadth queryString (autors) GET" ::Text)
         {----------------------------------------------------------------}
         "POST" -> case fmap join (lookupStuff ["user_id_autors","description","news_id"] req) of 
                [Just x,y,z] -> dbQueryInsert (Autors {user_id_autors= (read . toString . fromStrict) x,description=fmap (DT.pack . toString . fromStrict) y,news_id=fmap (read . toString . fromStrict) z}) 
                                                                    (insertsqlAutor) someAutorsEncoder req respond
                _ ->   respond $ responseOkJSON $ encode ("Problem wiadth queryString (autors) POST" ::Text)
         {----------------------------------------------------------------}
         "DELETE" -> case queryString req of
                   ([("user_id_autors",Just x)]) -> dbQueryInsert (Autors {user_id_autors= read (toString $ fromStrict x),description=Nothing,news_id=Nothing}) 
                                               (deletesql x) someAutorsEncoder req respond
                   _ ->   respond $ responseOkJSON $ encode ("Problem wiadth queryString (autors) DELETE" ::Text)
                   where 
                           deletesql x               = BS.pack $ "DELETE FROM autors WHERE user_id = $1;"
{- -- *!    "PUT"  -}
         _ ->  respond $ responseNotFoundJSON $ encode ("Problem with rqstMethod (autors)" ::Text)
  {------------------------------------------------------------}
  | otherwise = respond $ responseNotFoundJSON role

routeDrafts :: ByteString -> Application
routeDrafts role req respond = 
        case role of
           "\"Autor\"" -> 
               case pathInfo req of 
                 --(_:"drafts":"publish":_) -> case fmap join (lookupStuff ["id_of_draft","text_of_draft"] req)
                {---------------------------------------------}
                 _ -> case requestMethod req of 
                          "GET" -> dbQuery someDraftsDecoder HE.noParams (selectsql req) req respond
                          {-----------------------------------------------------------------}
                          "POST" -> case fmap join (lookupStuff ["id_of_draft","text_of_draft"] req) of 
                                    ([(Just x),(Just z)]) -> dbQueryInsert (Draft {id_of_draft= read (toString $ fromStrict x),id_of_user=read $ getId req,text_of_draft=DT.pack (toString $ fromStrict z)}) 
                                                                    (insertsqlDraft) someDraftsEncoder req respond
                                    _ -> respond $ responseOkJSON $ encode ("Problem wiadth queryString (drafts) POST" ::Text)
                          {----------------------------------------------------------------------}
                          "PUT" -> 
                                   case fmap join (lookupStuff ["id_of_draft","text_of_draft"] req) of 
                                   ([(Just x),(Just z)]) -> dbQueryInsert (Draft {id_of_draft= read (toString $ fromStrict x),id_of_user=read $ getId req,text_of_draft=DT.pack (toString $ fromStrict z)}) 
                                                                    (updateqlDraft) someDraftsEncoder req respond
                                   _ -> respond $ responseOkJSON $ encode ("Problem wiadth queryString (drafts) POST" ::Text)
                          {----------------------------------------------------------------------}
                          "DELETE" -> case fmap join (lookupStuff ["id_of_draft"] req) of 
                                      ([(Just x)]) -> dbQueryInsert (Draft {id_of_draft= read (toString $ fromStrict x),id_of_user=read $ getId req,text_of_draft=""}) 
                                                                    (deleteSqlDraft) someDraftsEncoder req respond
                                      _ -> respond $ responseOkJSON $ encode ("Problem wiadth queryString (drafts) POST" ::Text)
                                      where deleteSqlDraft = BS.pack $ "DELETE FROM drafts WHERE id_of_draft = $1;"
          {------------------------------------------}
           _ -> respond $ responseNotFoundJSON $ role 


{-help function for routeDrafts-}
selectsql req = (BS.pack $ "SELECT * FROM drafts WHERE user_id=" ++ (getId req) ++";")
getId req = fromMaybe (fmap (Prelude.lookup "user_id") $ jwtCheck (route req))
fromMaybe :: ToJSON a => Maybe (Maybe a) -> String
fromMaybe (Just(Just x)) = toString $ DA.encode x

lookupStuff [] lst = []
lookupStuff (stuff:s) lst = (Prelude.lookup stuff $ queryString lst) : (lookupStuff s lst)
insertsqlAutor = BS.pack "INSERT INTO autors(user_id, description,news_id) VALUES ($1,$2,$3);"
insertsqlDraft = BS.pack "INSERT INTO drafts(id_of_draft,user_id,text_of_draft) VALUES ($1,$2,$3);"
updateqlDraft  = BS.pack "UPDATE drafts SET text_of_draft = $3 WHERE id_of_draft=$1;"



--routeUsers :: ByteString ->  Application
--routeComments ::  ByteString ->  Application

dbQuery :: ToJSON a => HD.Result (Vector a) -> HE.Params b -> BS.ByteString ->  Application
dbQuery dec enc sql req respond = do 
  res <- connectToDB sql dec
  (respond $ responseOkJSON
              (case rawQueryString req of
                _-> encode res))

dbQueryInsert :: a -> BS.ByteString -> HE.Params a ->  Application
dbQueryInsert data1 sql enc req respond = do 
   res <- connectToDB2 data1 sql enc
   (respond $ responseOkJSON
              (case rawQueryString req of
                _-> encode res))            


responseOkJSON,responseNotFoundJSON,responseBadRequestJSON :: ByteString -> Response
responseOkJSON = responsePlainTextJSON status200
responseNotFoundJSON = responsePlainTextJSON notFound404
responseBadRequestJSON = responsePlainTextJSON badRequest400

responsePlainTextJSON :: Status -> ByteString -> Response
responsePlainTextJSON = (`responseLBS` [(hContentType,  "application/json; charset=utf-8")])