{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where
{----------------------------------------}
import DB
import Types
--import JWT
import qualified Hasql.Decoders as HD (column, nullable, nonNullable, rowVector, rowList, noResult, Result,Row)
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
import Data.Vector (fromList)
import Control.Monad.Trans.Reader
import System.Environment (getArgs)
{----------------------------------------}

yoursecret :: BS.ByteString
yoursecret="SERVER_HASKELL"
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
       Just (Just role) -> applicationPath (DA.encode role) req respond
       _                -> respond $ responseBadRequestJSON $ DA.encode ("error: беды с токеном"::Text)
      ----------------------------

--help function for application
route :: Request -> Text
route req = Prelude.head $ helphead (pathInfo req)
jwtCheck :: Text -> (Maybe [(Text, Value)])
jwtCheck content =  fmap (toList . unClaimsMap . unregisteredClaims . JWT.claims) (JWT.decodeAndVerifySignature (HMACSecret yoursecret) content)
helphead [] = ["error: empty list"]
helphead (x:xs) = [x]


applicationPath :: ByteString -> Application
applicationPath role req respond = 
  case pathInfo req of
    (_:"news":id1:"comments":_) -> routeComments id1  role req respond
    (_:"news":_)                -> routeNews          role req respond   
    (_:"autors":_)              -> routeAutors        role req respond
    (_:"drafts":_)              -> routeDrafts        role req respond
    (_:"users":_)               -> routeUsers         role req respond
    (_:"tags":_)                -> routeTags          role req respond
    (_:"categories":_)          -> routeCategory      role req respond
    _                           -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error: no Path",error_id=Just 404} :: Types.Status News)

routeNews :: ByteString -> Application
routeNews role req respond = 
    case pathInfo req of
    [_,"news"] -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "GROUP BY n.id_of_new, c, a,cmt ;") req respond
    [_,"news","filter"] ->
        case queryString req of 
        ("date_of_create_at_gt",Just x):xs -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND n.date_of_create > '" ++ BS.unpack x ++ "' GROUP BY n.id_of_new, a,cmt, c;") req respond
        ("date_of_create_at_lt",Just x):xs -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND n.date_of_create < '" ++ BS.unpack x ++ "' GROUP BY n.id_of_new, a,cmt, c;") req respond
        ("name_of_autor", Just x):xs       -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND u.first_name = '" ++ BS.unpack x ++ "' GROUP BY n.id_of_new,cmt, a, c;") req respond
        ("category", Just x):xs            -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND n.category_id = '" ++ BS.unpack x ++ "' GROUP BY n.id_of_new,cmt, a, c;") req respond
        ("tag", Just x):xs                 -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND " ++ BS.unpack x ++ "=ANY(n.tags) GROUP BY n.id_of_new, a,cmt, c;") req respond
        ("tag_in", Just x):xs              -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND ARRAY" ++ BS.unpack x ++ "&& (n.tags) GROUP BY n.id_of_new,cmt, a, c;") req respond
        ("tag_all", Just x):xs             -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND ARRAY" ++ BS.unpack x ++ "=(n.tags)  GROUP BY n.id_of_new,cmt, a, c;") req respond
        ("name", Just x):xs                -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND (n.name) LIKE ('%" ++ BS.unpack x ++ "%'::text)  GROUP BY n.id_of_new,cmt, a, c;") req respond
        ("text_of_new", Just x):xs             -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND (n.text_of_new) LIKE ('%" ++ BS.unpack x ++ "%'::text)  GROUP BY n.id_of_new, a,cmt, c;") req respond
        _  -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error : no query filter (news)",error_id=Just 404} :: Types.Status News)
    {----------------------------------------------------------------}
    [_,"news","order"] ->
        case queryString req of
        ("date_of_create",Just x):xs   -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ " GROUP BY n.id_of_new, a,cmt, c ORDER BY n.date_of_create " ++ BS.unpack x ++ ";") req respond
        ("category",Just x):xs         -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ " GROUP BY n.id_of_new, a,cmt, c ORDER BY n.category_id " ++ BS.unpack x ++ ";") req respond
        ("name_of_autor", Just x):xs   -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ " AND n.autor_id = u.user_id GROUP BY n.id_of_new, a,cmt, u.first_name, c ORDER BY u.first_name " ++ BS.unpack x ++ ";") req respond
        ("amount_of_photo", Just x):xs -> undefined
        _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error : no query filter (news)",error_id=Just 404} :: Types.Status News)
    {----------------------------------------------------------------}
    [_,"news","search"] -> 
        case queryString req of
        ("content",Just x):xs          -> dbQuery someNewsDecoder someNewsEncoder (BS.pack $ selectSqlNews ++ "AND ((n.text_of_new) LIKE ('%" ++ BS.unpack x ++ "%'::text) OR (n.name) LIKE ('%" ++ BS.unpack x ++ "%'::text)) GROUP BY n.id_of_new, a,cmt, c;") req respond
        _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "error : no query search (news)",error_id=Just 404} :: Types.Status News)
    {----------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ encode ("error: path or query news" ::Text)

selectSqlNews :: String
selectSqlNews = 
  "SELECT n.name, c, (array_agg (t)), n.text_of_new, n.id_of_new, a ,n.date_of_create, (array_agg(cmt)) " ++ 
       "FROM news n," ++
       "tags t," ++
       "categories c," ++
       "users u, " ++
       "(SELECT description, news_id,u FROM autors a, users u, news n WHERE u.user_id=a.user_id AND n.id_of_new = ANY(a.news_id)) as a " ++
       "INNER JOIN comments cmt ON cmt.id_of_new=id_of_new " ++
  "WHERE n.category_id = c.category_id AND u.user_id=n.autor_id AND t.tag_id = ANY(n.tags) AND n.autor_id=(a.u).user_id AND cmt.id_of_new= n.id_of_new "

routeAutors ::  ByteString -> Application
routeAutors role req respond  =
  case role of
    "\"Admin\"" ->
       case requestMethod req of
         "GET"  -> dbQuery (someAutorsDecoderNotNested) someAutorsEncoder (BS.pack selectSqlAutors) req respond
                
         {----------------------------------------------------------------}
         "POST" -> case fmap join (lookupStuff ["description","news_id"] req) of 
                [y,z] -> dbQueryInsert (AutorsEncoder {user_id_autors_en=0,description_en=fmap (DT.pack . toString . fromStrict) y,news_id_en=fmap ((\x -> read x ::[Integer]) . toString . fromStrict) z}) 
                                                                    (insertsqlAutor) someAutorsEncoderNotNested req respond
                _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (autors) POST",error_id=Just 404} :: Types.Status News) 
         {----------------------------------------------------------------}
         "DELETE" -> case queryString req of
                   ([("user_id_autors",Just x)]) -> dbQueryInsert (AutorsEncoder {user_id_autors_en=(read . toString . fromStrict) x,description_en=Nothing,news_id_en=Nothing}) 
                                               (deletesql x) someAutorsEncoderNotNested req respond
                   _ ->  respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (autors) DELETE",error_id=Just 404} :: Types.Status News) 
                   where 
                           deletesql x               = BS.pack $ "DELETE FROM autors WHERE user_id = $1;"
{- -- *!    "PUT"  -}
         _ ->  respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with rqstMethod (autors)",error_id=Just 404} :: Types.Status News) 
  {------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ B.pack $ "role is " ++ (B.unpack role)
selectSqlAutors :: String
selectSqlAutors = "SELECT a.description, a.news_id, u FROM autors a, users u, news n WHERE u.user_id=a.user_id  AND n.id_of_new = ANY(a.news_id);"
  
routeDrafts :: ByteString -> Application
routeDrafts role req respond = 
        case role of
           "\"Autor\"" -> 
               case pathInfo req of 
                --WARNING
                (_:"drafts":"publish":_) -> case (Prelude.lookup "id_of_draft" (queryString req)) of
                                              (Just x) -> dbQueryInsert () insertsqlNews HE.noParams req respond
                                              _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with queryString (drafts) POST",error_id=Just 404} :: Types.Status News)  
                {---------------------------------------------}
                _ -> case requestMethod req of 
                          "GET" -> dbQuery someDraftsDecoder HE.noParams (selectsqlDraft req) req respond
                          {-----------------------------------------------------------------}
                          "POST" -> case fmap join (lookupStuff ["text_of_draft"] req) of 
                                    ([(Just z)]) -> dbQueryInsert (Draft {id_of_draft= 0,id_of_user=read $ getId req,text_of_draft=DT.pack (toString $ fromStrict z)}) 
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
      | requestMethod req == "GET"    = dbQuery someCommentsDecoderNotNested someCommentsEncoder (selectSqlComment $ DT.unpack idNew) req respond
      {----------------------------------------------------------------------------}
      | requestMethod req == "DELETE" = 
                      case join (Prelude.lookup "id_of_comment" (queryString req)) of 
                      (Just idComment) -> 
                                     case role of
                                     "\"Admin\"" -> dbQueryInsert () (deleteSqlComment1 $ BS.unpack idComment) HE.noParams req respond  
                                     "\"Autor\"" -> dbQuery someCommentsDecoderNotNested someCommentsEncoder (deleteSqlComment2 ( BS.unpack idComment) $ getId req) req respond  
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
      | requestMethod req == "GET"  = dbQuery (someUsersDecoderNotNested) someUsersEncoder (selectSqlUser) req respond
      | requestMethod req == "POST" = case fmap join (lookupStuff ["first_name","second_name","image"] req) of 
                                      [Just first,Just second,image] -> dbQueryInsert (Users {image=fmap (DT.pack . BS.unpack) image,date_of_create_user=fromGregorian 0 0 0,user_id=0,first_name=DT.pack $ BS.unpack first,second_name=DT.pack $ BS.unpack second}) insertsqlUsers (someUsersEncoder) req respond 
                                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) POST",error_id=Just 404} :: Types.Status Comments)
      | requestMethod req == "DELETE" =
                              case role of
                              "\"Admin\"" ->  case join $ Prelude.lookup "user_id" (queryString req) of 
                                              (Just user) -> dbQueryInsert () (deleteSqlUser user) HE.noParams req respond 
                                              _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) DELETE",error_id=Just 404} :: Types.Status Comments)
                              _ -> respond $ responseNotFoundJSON $ role 

      | otherwise = respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with reqMethod (users)",error_id=Just 404} :: Types.Status Comments) 



routeTags :: ByteString -> Application
routeTags role req respond 
      | requestMethod req == "GET"  = dbQuery (someTagsDecoderNotNested) someTagsEncoder ("SELECT * FROM tags") req respond
      | requestMethod req == "POST" = case fmap join (lookupStuff ["text_of_tag"] req) of 
                                      [Just tag] -> dbQueryInsert (Tags{tag_name= (DT.pack . BS.unpack) tag,tag_id=0}) ("INSERT INTO tags(tag_id,tag_name) VALUES (DEFAULT,$2);") (someTagsEncoderNotNested) req respond 
                                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) POST",error_id=Just 404} :: Types.Status Comments)
      | requestMethod req == "DELETE" =   
                              case role of
                              "\"Admin\"" -> case join $ Prelude.lookup "tag_id" (queryString req) of 
                                             (Just tag) -> dbQueryInsert () (BS.pack $ "DELETE FROM users WHERE user_id = " ++ BS.unpack tag) HE.noParams req respond 
                                             _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) DELETE",error_id=Just 404} :: Types.Status Comments)
                              _ -> respond $ responseNotFoundJSON $ role 
      | requestMethod req == "PUT" =   
                      case role of
                      "\"Admin\"" -> case fmap join (lookupStuff ["id_of_tag","text_of_tag"] req) of 
                                             (Just tagId:Just tagText:_) -> dbQueryInsert (Tags{tag_name=DT.pack $ toString $ fromStrict $ tagText,tag_id=((\x -> read x ::Integer) . toString . fromStrict) tagId}) ("UPDATE tags SET tag_name = $1 WHERE tag_id=$2;") (someTagsEncoderNotNested) req respond 
                                             _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) DELETE",error_id=Just 404} :: Types.Status Comments)
                      _ -> respond $ responseNotFoundJSON $ role      
      | otherwise = respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with reqMethod (users)",error_id=Just 404} :: Types.Status Comments) 

routeCategory:: ByteString -> Application
routeCategory role req respond 
      | requestMethod req == "GET"  = dbQuery someCategoriesDecoderNotNested someTagsEncoder ("SELECT * FROM categories") req respond
      | requestMethod req == "POST" = case fmap join (lookupStuff ["parent_id","category_name"] req) of 
                                      (catId:Just catName:_) -> dbQueryInsert (Category{parent_id=fmap ((\x -> read x ::Integer) . toString . fromStrict) catId,category_name=DT.pack $ toString $ fromStrict $ catName,category_id=0}) ("INSERT INTO categories(parent_id,category_name,category_id) VALUES ($1,$2,DEFAULT);") (someCategoriesEncoder) req respond 
                                      _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) POST",error_id=Just 404} :: Types.Status Comments)
      | requestMethod req == "DELETE" =   
                              case role of
                              "\"Admin\"" -> case join $ Prelude.lookup "category_id" (queryString req) of 
                                             (Just category) -> dbQueryInsert () (BS.pack $ "DELETE FROM categories WHERE user_id = " ++ BS.unpack category) HE.noParams req respond 
                                             _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) DELETE",error_id=Just 404} :: Types.Status Comments)
                              _ -> respond $ responseNotFoundJSON $ role 
      | requestMethod req == "PUT" =   
                      case role of
                      "\"Admin\"" -> case fmap join (lookupStuff ["category_id","category_name"] req) of 
                                             (Just categoryId:Just categoryText:_) -> dbQueryInsert (Category{parent_id=Nothing,category_name=DT.pack $ toString $ fromStrict $ categoryText,category_id=((\x -> read x ::Integer) . toString . fromStrict) categoryId}) ("UPDATE categories SET category_name = $2 WHERE category_id=$3;") (someCategoriesEncoder) req respond 
                                             _ -> respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just $ "Problem with no query (users) DELETE",error_id=Just 404} :: Types.Status Comments)
                      _ -> respond $ responseNotFoundJSON $ role      
      | otherwise = respond $ responseNotFoundJSON $ encode (Status {ok=False,result=Nothing,error_description=Just "Problem with reqMethod (users)",error_id=Just 404} :: Types.Status Comments) 



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
insertsqlAutor     = BS.pack "INSERT INTO autors(user_id, description,news_id) VALUES (DEFAULT,$2,$3);"
insertsqlDraft     = BS.pack "INSERT INTO drafts(id_of_draft,user_id,text_of_draft) VALUES (DEFAULT,$2,$3);"
insertsqlNews      = BS.pack "UPDATE news n SET text_of_new = d.text_of_draft FROM drafts d WHERE n.id_of_new=2 and d.id_of_draft=4 ; "
updateqlDraft      = BS.pack "UPDATE drafts SET text_of_draft = $3 WHERE id_of_draft=$1;"
insertSqlComment   = BS.pack "INSERT INTO comments(id_of_comment,id_of_user,id_of_new,text_of_comment) VALUES (DEFAULT,$2,$3,$4)"
selectSqlUser      = BS.pack "SELECT * FROM users;"
insertsqlUsers     = BS.pack "INSERT INTO users(image,date_of_create,user_id,first_name,second_name) VALUES ($1,DEFAULT,DEFAULT,$4,$5)"
deleteSqlUser user = BS.pack $ "DELETE FROM users WHERE user_id = " ++ BS.unpack user ++ ";"

deleteSqlDraft :: Request -> BS.ByteString
deleteSqlDraft req = BS.pack $ "DELETE FROM drafts WHERE id_of_draft = " ++ (getId req) ++ ";"
deleteSqlComment1 idOfComment = BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++ idOfComment
deleteSqlComment2 :: [Char] -> [Char] -> BS.ByteString
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