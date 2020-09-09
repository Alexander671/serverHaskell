{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
{----------------------------------------}
import DB
import Types
--import JWT
import qualified Hasql.Decoders as HD (column, nullable, rowVector, rowList, noResult, Result,Row)
import qualified Hasql.Encoders as HE (noParams, Params)
{----------------------------------------}
import Web.JWT as JWT (ClaimsMap, unregisteredClaims, 
                       claims, header, unClaimsMap, signature,
                       Signer(HMACSecret), decode,decodeAndVerifySignature) 
{----------------------------------------}
import qualified Data.ByteString.Char8    as BS 
import qualified Data.ByteString.Lazy.Char8 as B (unpack,pack)
import           Data.ByteString.Lazy     (toStrict, fromStrict, ByteString)
import           Data.ByteString.Lazy.UTF8
import           Data.Map.Strict          (toList, Map, keys, lookup) 
import           Data.Text as DT          ( Text, pack, unpack ) 
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
import Data.Time  
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
       _                -> respond $ responseBadRequestJSON $ DA.encode ("error: беды с токеном"::Text)
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
    (_:"news":id1:"comments":_) -> routeComments id1  role req respond
    (_:"news":_)                -> routeNews          role req respond   
    (_:"autors":_)              -> routeAutors        role req respond
    (_:"drafts":_)              -> routeDrafts        role req respond
    (_:"users":_)               -> routeUsers         role req respond
    --(_:"tags":_)     -> routeTags     role req respond
    --(_:"category":_) -> routeCategory role req respond
    _                           -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error: no Path",error_id=Just 404} :: Types.Status News)

routeNews :: ByteString -> Application
routeNews role req respond = 
    case pathInfo req of
    [_,"news"] -> dbQuery someNewsDecoder someNewsEncoder "SELECT n.name, c, t, n.text_of_new, id_of_new, a ,date_of_create  FROM news n, tags t, categories c, autors a WHERE n.category_id = c.category_id AND a.autor_id=n.autor_id AND t.tag_id = ALL (n.tags)" req respond
    [_,"news","filter"] ->
        case queryString req of 
        ("date_of_create_at_gt",Just x):xs -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news WHERE date_of_create > '" ++ BS.unpack x ++ "';") req respond
        ("date_of_create_at_lt",Just x):xs -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news WHERE date_of_create < '" ++ BS.unpack x ++ "';") req respond
        ("name_of_autor", Just x):xs       -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  AND u.first_name = '" ++ BS.unpack x ++ "';") req respond
        ("category", Just x):xs            -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news WHERE category = '" ++ BS.unpack x ++ "';") req respond
{-
--*!      category_id
--*!      tag_id
--*!      in,all,any
-}      
        _  -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error : no query filter (news)",error_id=Just 404} :: Types.Status News)
    {----------------------------------------------------------------}
    [_,"news","order"] ->
        case queryString req of
        ("date_of_create",Just x):xs  -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news ORDER BY date_of_create " ++ BS.unpack x ++ ";") req respond
        ("category",Just x):xs        -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT * FROM news ORDER BY category " ++ BS.unpack x ++ ";") req respond
        ("name_of_autor", Just x):xs  -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ "SELECT n.*, u.second_name, u.first_name FROM news n, users u WHERE n.autor_id = u.user_id  ORDER BY u.first_name " ++ BS.unpack x ++ ";") req respond
{-
--*!      amount of photo
-}
{-
--*! search string (text_of_new,first_name,tag,category)
-}    
        _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error : no query filter (news)",error_id=Just 404} :: Types.Status News)
    {------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ encode ("error: path or query news" ::Text)

routeAutors ::  ByteString -> Application
routeAutors role req respond  =
  case role of
    "\"Admin\"" ->
       case requestMethod req of
         "GET"  ->  case pathInfo req of  
                     [_,"autors"] -> dbQuery (HD.rowVector $ HD.column $ HD.nullable someAutorsDecoder) someAutorsEncoder "SELECT * FROM autors;" req respond
                     _            -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (autors) GET",error_id=Just 404} :: Types.Status News) 
         {----------------------------------------------------------------}
         "POST" -> case fmap join (lookupStuff ["user_id_autors","description","news_id"] req) of 
                [Just x,y,z] -> dbQueryInsert (Autors {user_id_autors= (read . toString . fromStrict) x,description=fmap (DT.pack . toString . fromStrict) y,news_id=fmap (read . toString . fromStrict) z}) 
                                                                    (insertsqlAutor) someAutorsEncoder req respond
                _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (autors) POST",error_id=Just 404} :: Types.Status News) 
         {----------------------------------------------------------------}
         "DELETE" -> case queryString req of
                   ([("user_id_autors",Just x)]) -> dbQueryInsert (Autors {user_id_autors= read (toString $ fromStrict x),description=Nothing,news_id=Nothing}) 
                                               (deletesql x) someAutorsEncoder req respond
                   _ ->  respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (autors) DELETE",error_id=Just 404} :: Types.Status News) 
                   where 
                           deletesql x               = BS.pack $ "DELETE FROM autors WHERE user_id = $1;"
{- -- *!    "PUT"  -}
         _ ->  respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with rqstMethod (autors)",error_id=Just 404} :: Types.Status News) 
  {------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ B.pack $ "role is " ++ (B.unpack role)

{- -- *!    "добавить проверку на условие черновиков конкретного автора"  -}
routeDrafts :: ByteString -> Application
routeDrafts role req respond = 
        case role of
           "\"Autor\"" -> 
               case pathInfo req of 
                (_:"drafts":"publish":_) -> case (Prelude.lookup "id_of_draft" (queryString req)) of
                                              (Just x) -> dbQueryInsert ()
                                               (mappend insertsqlNews $ deleteSqlDraft req) HE.noParams req respond
                                              _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (drafts) POST",error_id=Just 404} :: Types.Status News)  
                {---------------------------------------------}
                _ -> case requestMethod req of 
                          "GET" -> dbQuery someDraftsDecoder HE.noParams (selectsqlDraft req) req respond
                          {-----------------------------------------------------------------}
                          "POST" -> case fmap join (lookupStuff ["id_of_draft","text_of_draft"] req) of 
                                    ([(Just x),(Just z)]) -> dbQueryInsert (Draft {id_of_draft= read (toString $ fromStrict x),id_of_user=read $ getId req,text_of_draft=DT.pack (toString $ fromStrict z)}) 
                                                                    (insertsqlDraft) someDraftsEncoder req respond
                                    _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (drafts) POST",error_id=Just 404} :: Types.Status News)  
                          {----------------------------------------------------------------------}
                          "PUT" ->  case fmap join (lookupStuff ["id_of_draft","text_of_draft"] req) of 
                                    ([(Just x),(Just z)]) -> dbQueryInsert (Draft {id_of_draft= read (toString $ fromStrict x),id_of_user=read $ getId req,text_of_draft=DT.pack (toString $ fromStrict z)}) 
                                                                    (updateqlDraft) someDraftsEncoder req respond
                                    _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (drafts) PUT",error_id=Just 404} :: Types.Status News)  
                          {----------------------------------------------------------------------}
                          "DELETE" -> case fmap join (lookupStuff ["id_of_draft"] req) of 
                                      ([(Just x)]) -> dbQueryInsert () (deleteSqlDraft req) HE.noParams req respond
                                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (drafts) DELETE",error_id=Just 404} :: Types.Status News)  
          {------------------------------------------}
           _ -> respond $ responseNotFoundJSON $ role 

routeComments :: Text -> ByteString -> Application
routeComments idNew role req respond 
      | requestMethod req == "GET"    = dbQuery someCommentsDecoder someCommentsEncoder (selectSqlComment $ DT.unpack idNew) req respond
      {----------------------------------------------------------------------------}
      | requestMethod req == "DELETE" = 
                      case join (Prelude.lookup "id_of_comment" (queryString req)) of 
                      (Just idComment) -> 
                                     case role of
                                     "\"Admin\"" -> dbQueryInsert () (deleteSqlComment1 $ BS.unpack idComment) HE.noParams req respond  
                                     "\"Autor\"" -> dbQuery someCommentsDecoder someCommentsEncoder (deleteSqlComment2 ( BS.unpack idComment) $ getId req) req respond  
                                     _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with role (comments)",error_id=Just 404} :: Types.Status Comments) 
                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with no query (comments)",error_id=Just 404} :: Types.Status Comments) 
      {----------------------------------------------------------------------------}
      | requestMethod req == "POST"    = 
                      case join (Prelude.lookup "text_of_comment" (queryString req)) of 
                      (Just comment) -> dbQueryInsert (Comments {id_of_comment=0,user_id_comment=read $ getId req,id_of_new_comment=read $ DT.unpack idNew,text_of_comment=DT.pack $ BS.unpack comment}) (insertSqlComment) someCommentsEncoder  req respond  
                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with no query (comments)",error_id=Just 404} :: Types.Status Comments)    
      {----------------------------------------------------------------------------}
      | otherwise = respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with reqMethod (comments)",error_id=Just 404} :: Types.Status Comments) 
 
routeUsers :: ByteString -> Application
routeUsers role req respond 
      | requestMethod req == "GET"  = dbQuery someUsersDecoder someUsersEncoder (selectSqlUser) req respond
      | requestMethod req == "POST" = case fmap join (lookupStuff ["first_name","second_name","image"] req) of 
                                      [Just first,Just second,image] -> dbQueryInsert (Users {image=fmap (DT.pack . BS.unpack) image,date_of_create_user=fromGregorian 0 0 0,user_id=0,first_name=DT.pack $ BS.unpack first,second_name=DT.pack $ BS.unpack second}) insertsqlUsers someUsersEncoder req respond 
                                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) POST",error_id=Just 404} :: Types.Status Comments)
      | requestMethod req == "DELETE"   &&
        role              == "\"Admin\"" = case join $ Prelude.lookup "user_id" (queryString req) of 
                                      (Just user) -> dbQueryInsert () (deleteSqlUser user) HE.noParams req respond 
                                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) DELETE",error_id=Just 404} :: Types.Status Comments)
      | otherwise = respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with reqMethod (users)",error_id=Just 404} :: Types.Status Comments) 
 


routeTags :: ByteString -> Application
routeTags role req respond = undefined


{-help function for route-}
selectsqlDraft req = BS.pack $ "SELECT * FROM drafts WHERE user_id=" ++ (getId req) ++";"
getId req = fromMaybe (fmap (Prelude.lookup "user_id") $ jwtCheck (route req))
fromMaybe :: ToJSON a => Maybe (Maybe a) -> String
fromMaybe (Just(Just x)) = toString $ DA.encode x
fromMaybe (Just Nothing) = ""
fromMaybe Nothing        = ""

lookupStuff :: [BS.ByteString] -> Request -> [Maybe (Maybe BS.ByteString)]
lookupStuff [] lst = []
lookupStuff (stuff:s) lst = (Prelude.lookup stuff $ queryString lst) : (lookupStuff s lst)
insertsqlAutor,insertsqlDraft,updateqlDraft,insertsqlNews,insertSqlComment,selectSqlUser,insertsqlUsers  :: BS.ByteString
insertsqlAutor     = BS.pack "INSERT INTO autors(user_id, description,news_id) VALUES ($1,$2,$3);"
insertsqlDraft     = BS.pack "INSERT INTO drafts(id_of_draft,user_id,text_of_draft) VALUES ($1,$2,$3);"
insertsqlNews      = BS.pack "UPDATE news n SET text_of_new = d.text_of_draft FROM drafts d WHERE n.id_of_new=2 and d.id_of_draft=4 ; "
updateqlDraft      = BS.pack "UPDATE drafts SET text_of_draft = $3 WHERE id_of_draft=$1;"
insertSqlComment   = BS.pack "INSERT INTO comments(id_of_comment,id_of_user,id_of_new,text_of_comment) VALUES (DEFAULT,$2,$3,$4)"
selectSqlUser      = BS.pack "SELECT * FROM users;"
insertsqlUsers     = BS.pack "INSERT INTO users(image,date_of_create,user_id,first_name,second_name) VALUES ($1,DEFAULT,DEFAULT,$4,$5)"
deleteSqlUser user = BS.pack $ "DELETE FROM users WHERE user_id = " ++ BS.unpack user ++ ";"

deleteSqlDraft :: Request -> BS.ByteString
deleteSqlDraft req = BS.pack $ "DELETE FROM drafts WHERE id_of_draft = " ++ (getId req) ++ ";"
deleteSqlComment1 idOfComment = BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++ idOfComment
deleteSqlComment2 idOfNew idOfComment = BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++ idOfComment ++ " AND id_of_new = " ++ idOfNew ++ ";"
selectSqlComment id1 = BS.pack $ "SELECT * FROM comments WHERE id_of_new =" ++ id1 ++ " ;" 
--routeUsers :: ByteString ->  Application
--routeComments ::  ByteString ->  Application

dbQuery :: ToJSON a => HD.Result (Vector a) -> HE.Params b -> BS.ByteString ->  Application
dbQuery dec enc sql req respond = do 
  res <- connectToDB sql dec
  (respond $ responseOkJSON
              (case res of
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

responsePlainTextJSON :: Network.HTTP.Types.Status -> ByteString -> Response
responsePlainTextJSON = (`responseLBS` [(hContentType,  "application/json; charset=utf-8")])