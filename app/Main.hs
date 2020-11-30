{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import API (statusError)
import Types
import DB
import JWT (getId, route, jwtCheck, yoursecret, takeRoleJWT)
import qualified Hasql.Decoders as HD (text, nonNullable, rowVector, column, int8, Result)
import qualified Hasql.Encoders as HE (Params, noParams)
import Logging.Level (LogLevel(INFO))
import Logging.Logging as L (runLog)
import Web.JWT as JWT
    ( encodeSigned,
      hmacSecret,
      ClaimsMap(ClaimsMap, unClaimsMap),
      JWTClaimsSet(unregisteredClaims) )
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as B (pack, unpack)
import Data.ByteString.Lazy.UTF8 ( ByteString, toString )
import Data.Text as DT (empty, pack, unpack)
import Network.HTTP.Types
  ( Status
  , badRequest400
  , hContentType
  , notFound404
  , status200
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , pathInfo
  , queryString
  , rawQueryString
  , requestMethod
  , responseLBS
  )
import Network.Wai.Handler.Warp (run)
import Control.Monad (join)
import Data.Aeson as DA (toJSON, Value(String), ToJSON, encode)
import Data.Time (fromGregorian)
import Data.Vector as DV (Vector,head)
import qualified Data.Map as Map
import Text.Read ( readMaybe )
import Types


type Role = String
type SQL = BS.ByteString
type Message = String
data Route = 
              EncoderLogInUp   SQL LogInUp
            | DecoderLogInUp   SQL Role
            | RouteRegistration Role String LogInUp SQL
            | DecoderComments  SQL Role 
            | EncoderComments  SQL CommentsNotNested
            | DecoderNews      SQL Role
            | EncoderNotNestNews SQL NewsEncoderNotNested
            | EncoderNews      SQL SQL SQL NewsEncoder
            | EncoderAutors    SQL AutorsEncoder
            | DecoderAutors    SQL Role
            | DecoderMe        SQL Role
            | EncoderUsers     SQL Users
            | DecoderUsers     SQL Role
            | EncoderTags      SQL Tags
            | DecoderTags      SQL Role
            | EncoderCategories SQL Category
            | DecoderCategories SQL Role
            | EncoderNoParams  SQL
            | Error Message

-- Main function
main :: IO ()
main = do
  L.runLog INFO "Server is run.."
  run 8000 application

application :: Application
application req respond = let roleJWT = takeRoleJWT req
                              route   = routePath req roleJWT
                          in dbQueryMain route req respond
 
---------------------------------------
routePath :: Request -> ByteString -> Route
routePath req role =
  case pathInfo req of
    ("registration":_)            -> routeRegistration role req 
    ("login":_)                   -> routeLogin role   req 
    (_:_:"news":id1:"comments":_) -> routeComments id1 role  req 
    (_:_:"news":_)                -> routeNews role  req    
    (_:_:"autors":_)              -> routeAutors role req 
    (_:_:"drafts":_)              -> routeDrafts role req
    (_:_:"users":"me":_)          -> routeMe role req   
    (_:_:"users":_)               -> routeUsers role req   
    (_:_:"tags":_)                -> routeTags role req   
    (_:_:"categories":_)          -> routeCategory role req
    _ -> Error ""



dbQueryMain :: Route -> Application
dbQueryMain (DecoderLogInUp sql role)   req respond = dbQuery role (HD.rowVector $ Types.Text1 <$> HD.column (HD.nonNullable HD.text)) sql req respond
dbQueryMain (EncoderLogInUp sql data1)  req respond = dbQueryInsert data1 sql someLogInUpEncoder req respond   
dbQueryMain (RouteRegistration role log a sql) req respond = dbQueryReg role log a sql someLogInUpEncoder req respond
dbQueryMain (DecoderComments  sql role) req respond = dbQuery role someCommentsDecoderNotNested sql req respond
dbQueryMain (EncoderComments  sql data1)req respond = dbQueryInsert data1 sql someCommentsEncoderNotNested req respond
dbQueryMain (DecoderNews      sql role) req respond = dbQuery role someNewsDecoder sql req respond
dbQueryMain (EncoderNotNestNews sql data1)req respond = dbQueryInsert data1 sql someNewsEncoderNotNested req respond
dbQueryMain (EncoderNews      sql1 sql2 sql3 data1)req respond = dbQueryDrafts data1 sql1 sql2 sql3 someNewsEncoder req respond
dbQueryMain (DecoderAutors    sql role) req respond = dbQuery role someAutorsDecoderNotNested sql req respond
dbQueryMain (EncoderAutors    sql data1)req respond = dbQueryInsert data1 sql someAutorsEncoder req respond
dbQueryMain (DecoderUsers     sql role) req respond = dbQuery role someUsersDecoderNotNested sql req respond   
dbQueryMain (EncoderUsers     sql data1)req respond = dbQueryInsert data1 sql someUsersEncoder req respond
dbQueryMain (DecoderMe        sql role) req respond = dbQuery role  someUsersDecoderNotNested sql req respond
dbQueryMain (EncoderTags      sql data1)req respond = dbQueryInsert data1 sql someTagsEncoderNotNested req respond
dbQueryMain (DecoderTags      sql role) req respond = dbQuery role someTagsDecoderNotNested sql req respond   
dbQueryMain (EncoderCategories sql data1)req respond= dbQueryInsert data1 sql someCategoriesEncoder req respond
dbQueryMain (DecoderCategories sql role)req respond = dbQuery role someCategoriesDecoderNotNested sql req respond   
dbQueryMain (EncoderNoParams  sql)      req respond = dbQueryInsert () sql HE.noParams req respond  
dbQueryMain (Error message)             req respond = respond $ responseNotFoundJSON $ encode message

   
dbQuery :: ToJSON a => String -> HD.Result (Vector a) -> BS.ByteString -> Application
dbQuery role dec sql req respond = do
  res <- connectToDB role (BS.pack (BS.unpack sql ++ limitOffset req)) dec
  respond $ responseOkJSON $ encode res
        
limitOffset :: Request -> String
limitOffset req = fromJust $ fmap (show . (5 *)) (readMaybe (pagin req)::Maybe Int)
        where fromJust Nothing  =""
              fromJust (Just x) =  " LIMIT 5 OFFSET " ++ x

pagin :: Request -> String
pagin req = (DT.unpack $ lastJust $ Prelude.take 2 $ (pathInfo req)) 
   where lastJust [] = "0"
         lastJust x  = last x

dbQueryInsert :: ToJSON a => a -> BS.ByteString -> HE.Params a -> Application
dbQueryInsert data1 sql enc req respond = do
  res <- connectToDB2 data1 sql enc
  respond $ responseOkJSON $ encode res


dbQueryPublish = undefined

routeRegistration :: ByteString -> Request -> Route
routeRegistration role req = 
  case requestMethod req of
    "POST" -> case fmap join (lookupStuff ["login", "password"] req) of
              (Just login:Just password:_) -> 
                RouteRegistration
                (toString role)
                (toString $ fromStrict login)
                (LogInUp
                   { user_id_reg = 0
                   , login = (DT.pack . toString . fromStrict) login
                   , password = (DT.pack . toString . fromStrict) password
                   , token = ""
                   })
                insertSqlRegistr
              _ -> Error ""
    _ -> Error ""


dbQueryReg role log data1 sql enc req respond = do
  res1 <- connectToDB2 data1 sql enc
  id1  <- connectToDB role (BS.pack $ "SELECT user_id FROM registration WHERE login = '" ++ log ++ "'") (HD.rowVector $ Types.Integer1 <$> (HD.column (HD.nonNullable $ fromIntegral <$> HD.int8)))        
  _    <- connectToDB2  data1 (BS.pack $ "UPDATE registration SET token = '" ++ (DT.unpack $ createToken $ int <$> (getId id1)) ++ "' WHERE user_id = " ++ (fromMaybe $ getId id1)) enc        
  (respond $
   responseOkJSON
     (case rawQueryString req of
        _ -> encode res1))
  where getId id1          = fmap DV.head $ result id1
        fromMaybe Nothing  = "NULL"
        fromMaybe (Just x) = show $ int x 
        createToken (Just x) = let
                        cs = mempty { 
                        unregisteredClaims = ClaimsMap $ Map.fromList [(DT.pack "user_id",toJSON x),(DT.pack "role",DA.String "User")]
                        }
                        key = hmacSecret $ DT.pack $ BS.unpack yoursecret
                        in encodeSigned key mempty cs
        createToken Nothing = DT.empty

insertSqlRegistr :: BS.ByteString
insertSqlRegistr = BS.pack "INSERT INTO registration(user_id,login,password,token) VALUES(DEFAULT,$2,$3,'')"


routeLogin role req = 
  case queryString req of
    [("login",Just login),("password",Just password)] -> DecoderLogInUp (selectSQLtoken login password) (toString role)    
    _ -> Error ""
    
selectSQLtoken :: BS.ByteString -> BS.ByteString -> BS.ByteString
selectSQLtoken login password = BS.pack $ "SELECT token FROM registration WHERE login = '" ++ BS.unpack login ++ "' AND password = '" ++ BS.unpack password ++ "'"

routeNews role req =
  case pathInfo req of
    [_,_, "news"] ->
      DecoderNews (BS.pack $ selectSqlNews ++ " ORDER BY nn.id_of_new DESC")
      (toString role)
    [_,_, "news", "filter"] ->
      case queryString req of
        ("date_of_create_at_gt", Just x):_ ->
          DecoderNews 
          (BS.pack $
             selectSqlNews ++
             "AND nn.date_of_create > '" ++ BS.unpack x ++ "' ")
          (toString role)
        ("date_of_create_at_lt", Just x):_ ->
          DecoderNews 
          (BS.pack $
             selectSqlNews ++
             "AND nn.date_of_create < '" ++ BS.unpack x ++ "' ")
          (toString role)
        ("name_of_autor", Just x):_ ->
          DecoderNews             (BS.pack $
             selectSqlNews ++
             "AND ((nn.a).u).first_name = '" ++ BS.unpack x ++ "' ")
          (toString role)
        ("category", Just x):_ ->
          DecoderNews             (BS.pack $
             selectSqlNews ++
             "AND (nn.categories).category_id = '" ++ BS.unpack x ++ "'  ")
          (toString role)
        ("tag", Just x):_ ->
          DecoderNews
              (BS.pack $
             "SELECT * FROM nestednews nn, unnest(nn.tags) as unntag WHERE unntag.tag_id=" ++
             BS.unpack x)
         (toString role)
        ("tag_in", Just x):_ ->
          DecoderNews
          (BS.pack $
             "SELECT * FROM nestednews nn, unnest(nn.tags) as unntag WHERE unntag.tag_id = ANY(ARRAY" ++
             BS.unpack x ++ ")")
          (toString role)
        ("tag_all", Just x):_ ->
          DecoderNews
            (BS.pack $
             "SELECT * FROM nestednews nn, unnest(nn.tags) as unntag WHERE unntag.tag_id = ALL(ARRAY" ++
             BS.unpack x ++ "::integer[]) ")
          (toString role) 
        ("name", Just x):_ ->
          DecoderNews
          (BS.pack $
             selectSqlNews ++
             "AND (nn.name) LIKE ('%" ++ BS.unpack x ++ "%'::text)  ")
          (toString role)   
        ("text_of_new", Just x):_ ->
          DecoderNews
          (BS.pack $
            selectSqlNews ++
            "AND (nn.text_of_new) LIKE ('%" ++ BS.unpack x ++ "%'::text) ")
         (toString role)
        ("id_of_new", Just x):_ ->
          DecoderNews     
          (BS.pack $
             selectSqlNews ++ "AND nn.id_of_new= " ++ BS.unpack x ++ " ")
          (toString role)
        _ -> Error ""
    {----------------------------------------------------------------}
    [_,_, "news", "order"] ->
      case queryString req of
        ("date_of_create", Just x):_ ->
          DecoderNews
          (BS.pack $
             selectSqlNews ++
             " ORDER BY nn.date_of_create " ++ BS.unpack x)
          (toString role)
        ("name_of_autor", Just x):_ ->
          DecoderNews 
            (BS.pack $
             selectSqlNews ++
             " ORDER BY ((nn.a).u).first_name " ++ BS.unpack x)
            (toString role)
        ("category", Just x):_ ->
          DecoderNews
            (BS.pack $
             selectSqlNews ++
             " ORDER BY (nn.categories).category_name " ++ BS.unpack x)
            (toString role) 
        ("amount_of_photo", Just "DESC"):_ ->
          DecoderNews 
            (BS.pack $ selectSqlNews ++ " ORDER BY array_length(images,2)")
            (toString role)
        ("amount_of_photo", Just "ASC"):_ ->
          DecoderNews 
            (BS.pack $ selectSqlNews ++ " ORDER BY array_length(images,1)")
            (toString role)
        _ -> Error ""
    {----------------------------------------------------------------}
    [_,_, "news", "search"] ->
      case queryString req of
        ("content", Just x):_ ->
          DecoderNews 
            (BS.pack $
             "SELECT * FROM nestedNewsSearch('%" ++ BS.unpack x ++ "%')")
            (toString role)
        _ -> Error ""
    {----------------------------------------------------------------}
    _ -> Error ""

selectSqlNews :: String
selectSqlNews = "SELECT * FROM nestednews nn WHERE true "


routeAutors role req =
  case role of
    "\"Admin\"" ->
      case requestMethod req of
        "GET" ->
          DecoderAutors 
            (BS.pack "SELECT * FROM nestedautor")
            (toString role)
          {----------------------------------------------------------------}
        "POST" ->
          case fmap join (lookupStuff ["user_id", "description"] req) of
            [Just x, y] ->
              EncoderAutors
                insertsqlAutor
                (AutorsEncoder
                   { user_id_autors_en = read (BS.unpack x) :: Integer
                   , description_en = fmap (DT.pack . toString . fromStrict) y
                   })
            _ -> Error ""
          {----------------------------------------------------------------}
        "DELETE" ->
          case queryString req of
            ([("user_id", Just x)]) ->
              EncoderAutors
                deletesqlAutor
                (AutorsEncoder
                   { user_id_autors_en = (read . toString . fromStrict) x
                   , description_en = Nothing
                   })
            _ -> Error ""
        "PUT" ->
          case queryString req of
            ([("description", x), ("user_id", Just desc)]) ->
              EncoderAutors
                putAutors
                (AutorsEncoder
                   { user_id_autors_en = (read . toString . fromStrict) desc
                   , description_en = fmap (DT.pack . toString . fromStrict) x
                   })
            _ -> Error ""
        _ -> Error ""
  {------------------------------------------------------------}
    _ -> Error ""

putAutors,insertsqlAutor,deletesqlAutor :: BS.ByteString
putAutors = BS.pack "UPDATE autors SET description = $1 WHERE user_id=$2"

insertsqlAutor = BS.pack "INSERT INTO autors(user_id, description) VALUES ($2,$1)"

deletesqlAutor = BS.pack "DELETE FROM autors WHERE user_id = $2"

routeCategory role req 
  | requestMethod req == "GET" =
    DecoderCategories 
      ("SELECT * FROM categories")
      (toString role)
  | role == "\"Admin\"" =
    case requestMethod req of
      "POST" ->
        case fmap join (lookupStuff ["parent_id", "category_name"] req) of
          (catId:Just catName:_) ->
            EncoderCategories
              "INSERT INTO categories(child_id,category_name,category_id) VALUES ($1,$2,DEFAULT)"
              (Category
                 { parent_id =
                     fmap
                       ((\x -> read x :: Integer) . toString . fromStrict)
                       catId
                 , category_name = DT.pack $ toString $ fromStrict $ catName
                 , category_id = 0
                 })
          _ ->Error ""
      "DELETE" ->
        case join $ Prelude.lookup "category_id" (queryString req) of
          (Just category) ->
            EncoderNoParams
              (BS.pack $ "DELETE FROM categories WHERE category_id = " ++ BS.unpack category)
          _ -> Error ""
      "PUT" ->
        case fmap join (lookupStuff ["category_id", "category_name","parent_id"] req) of
          (Just categoryId:Just categoryText:prntId:_) ->
            EncoderCategories
              "UPDATE categories SET category_name = $2, child_id = $1 WHERE category_id=$3"
              (Category
                 { parent_id = fmap ((\x -> read x :: Integer) . toString . fromStrict) prntId
                 , category_name =
                     DT.pack $ toString $ fromStrict $ categoryText
                 , category_id =
                     ((\x -> read x :: Integer) . toString . fromStrict)
                       categoryId
                 })
          _ -> Error ""
      _ -> Error ""
  | otherwise = Error ""

routeTags role req
    | requestMethod req == "GET" =
        DecoderTags 
        "SELECT * FROM tags"
        (toString role)
    | role == "\"Admin\"" =
      case requestMethod req of 
        "POST" ->
            case fmap join (lookupStuff ["text_of_tag"] req) of
              [Just tag] ->
                EncoderTags
                "INSERT INTO tags(tag_name,tag_id) VALUES ($1,DEFAULT)"
                (Tags {tag_name = (DT.pack . BS.unpack) tag, tag_id = 0})
              _ -> Error ""
        "DELETE" ->
            case join $ Prelude.lookup "tag_id" (queryString req) of
              (Just tag) ->
                EncoderNoParams
                (BS.pack $ "DELETE FROM users WHERE user_id = " ++ BS.unpack tag)
              _ -> Error ""
        "PUT" ->
          case fmap join (lookupStuff ["tag_id", "text_of_tag"] req) of
            (Just tagId:Just tagText:_) ->
              EncoderTags
              "UPDATE tags SET tag_name = $1 WHERE tag_id=$2"
              (Tags
              { tag_name = DT.pack $ toString $ fromStrict $ tagText
              , tag_id =((\x -> read x :: Integer) . toString . fromStrict) tagId
              })
            _ -> Error ""
        _ -> Error ""
    | otherwise = Error ""

routeDrafts role req =
  case role of
    "\"Autor\"" ->
      case pathInfo req of
        (_:_:"drafts":"publish":_) ->
          case (join $ Prelude.lookup "id_of_draft" (queryString req)) of
            (Just x) -> EncoderNoParams (publishsqlNews x)
            _ -> Error ""
                {---------------------------------------------}
        _ ->
          case requestMethod req of
            "GET" -> DecoderNews (selectsqlDraft req) (toString role)
                          {-----------------------------------------------------------------}
            "POST" ->
              case fmap join (lookupStuff ["name","text_of_draft","category_id","photo"] req) of
                [Just name,txt,Just cat,pht] ->
                  EncoderNotNestNews
                    insertsqlDraft
                    (NewsEncoderNotNested
                       { name_not = (DT.pack . BS.unpack) name,
                         text_of_new_not =fmap (DT.pack . BS.unpack) txt,
                         category_not = read (BS.unpack cat)::Integer,
                         photo_not = fmap (DT.pack . BS.unpack) pht,
                         autor_id_not = read (getId req) :: Integer
                       })
                _ -> Error ""
                          {----------------------------------------------------------------------}
            "PUT" ->
              case fmap join (lookupStuff ["draft_id","name","text_of_draft","category_id","photo","images","tags"] req) of
                [Just id1,name,txt,cat,pht,img,tgs] ->
                  EncoderNews
                    updatesqlDraft
                    updatesqlImages
                    updatesqlTags
                    (NewsEncoder
                       { draft_id =  read (BS.unpack id1)::Integer,
                         name_en = fmap (DT.pack . BS.unpack) name,
                         text_of_new_en =fmap (DT.pack . BS.unpack) txt,
                         category_en =fmap ((\x -> read x ::Integer) . BS.unpack) cat ,
                         photo_en = fmap (DT.pack . BS.unpack) pht,
                         images_en = fmap (\x -> read x::[Integer]) (fmap BS.unpack img),
                         tags_en = fmap (\x -> read x::[Integer]) (fmap BS.unpack tgs)                       
                       })
                _ -> Error ""
                          {----------------------------------------------------------------------}
            "DELETE" ->
              case fmap join (lookupStuff ["id_of_draft"] req) of
                [Just x] ->
                  EncoderNoParams (deleteSqlDraft x)
                _ -> Error ""
          {------------------------------------------}
    _ -> Error ""

dbQueryDrafts data1 sql1 sql2 sql3 enc req respond = do
  res1 <- connectToDB2 data1 sql1 enc
  res2 <- connectToDB2 data1 sql2 enc
  res3 <- connectToDB2 data1 sql3 enc
  (respond $
   responseOkJSON
     (case rawQueryString req of
        _ -> encode res1))

{-query for drafts-}
insertsqlDraft,updatesqlDraft :: BS.ByteString
insertsqlDraft =
  BS.pack
    "INSERT INTO news(name,text_of_new,id_of_new,date_of_create,category_id,photo,publish,autor_id) \
    \VALUES ($1,$2,DEFAULT,DEFAULT,$3,$4,false,$5) "

publishsqlNews :: BS.ByteString -> BS.ByteString
publishsqlNews id_of_new = BS.pack $ "UPDATE news SET publish = true WHERE id_of_new = " ++ BS.unpack id_of_new

updatesqlDraft =
  BS.pack "UPDATE news SET name=$2, text_of_new = $3, category_id=$4, photo=$5 WHERE id_of_new=$1 "

updatesqlImages =
  BS.pack "INSERT into images(id_of_image,id_of_new,image) \
  \VALUES(DEFAULT,$1,unnest($6))"

updatesqlTags =
  BS.pack "INSERT into news_tags(new_id,tag_id,global_tag_id) \
  \VALUES($1,unnest($7),DEFAULT)"

deleteSqlDraft id =
  BS.pack $ "DELETE FROM news WHERE id_of_new = " ++ BS.unpack id ++ " AND publish=false"

selectsqlDraft req =
  BS.pack $ "SELECT * FROM nestednews n WHERE (n.a).u.user_id=" ++ getId req ++ " AND n.publish=false"

{-------------------------------}

routeMe role req 
  | requestMethod req == "GET" =
        DecoderMe (selectSqlUserid $ getId req)
        (toString role)
  | otherwise = Error ""


routeUsers role req
  | requestMethod req == "GET" =
        DecoderUsers 
        selectSqlUser (toString role)
  | requestMethod req == "PUT" =
    case fmap join (lookupStuff ["first_name", "second_name", "image"] req) of
      [first, second, image] ->
        EncoderUsers
        updateSqlUser
          (Users
             { image = fmap (DT.pack . BS.unpack) image
             , date_of_create_user = fromGregorian 0 0 0
             , user_id = read (getId req) :: Integer
             , first_name = fmap (DT.pack . BS.unpack) first
             , second_name = fmap (DT.pack . BS.unpack) second
             })
      _ -> Error ""
  | requestMethod req == "DELETE" =
    case role of
      "\"Admin\"" ->
        case join $ Prelude.lookup "user_id" (queryString req) of
          (Just user) ->
            EncoderNoParams (deleteSqlUser $ BS.unpack user)
          _ -> Error ""
      _ -> Error ""
  | otherwise = Error ""

selectSqlUser,insertsqlUsers,updateSqlUser :: BS.ByteString
selectSqlUser = BS.pack "SELECT * FROM users"

insertsqlUsers = "INSERT INTO users(image,date_of_create,user_id,first_name,second_name) VALUES ($1,DEFAULT,DEFAULT,$4,$5)"

updateSqlUser = "UPDATE users SET first_name = $4, second_name=$5, image =$1 WHERE user_id=$3 "

selectSqlUserid,deleteSqlUser :: String -> BS.ByteString
selectSqlUserid x = BS.pack $ "SELECT * FROM users WHERE user_id = " ++ x 

deleteSqlUser user =
  BS.pack $ "DELETE FROM registration WHERE user_id = " ++ user 



routeComments idNew role req
  | requestMethod req == "GET" =
    DecoderComments (selectSqlComment $ DT.unpack idNew)    
    (toString role)
      {----------------------------------------------------------------------------}
  | requestMethod req == "DELETE" =
    case join (Prelude.lookup "id_of_comment" (queryString req)) of
      (Just idComment) ->
        case role of
          "\"Admin\"" ->
            EncoderNoParams
              (deleteSqlComment1 $ BS.unpack idComment)
          _ -> Error ""
      _ -> Error ""
      {----------------------------------------------------------------------------}
  | requestMethod req == "POST" =
    case join (Prelude.lookup "text_of_comment" (queryString req)) of
      (Just comment) ->
        EncoderComments
          insertSqlComment
          (CommentsNotNested
             { id_of_comment_nt = 0
             , user_id_comment_nt = read $ getId req
             , id_of_new_comment_nt = read $ DT.unpack idNew
             , text_of_comment_nt = DT.pack $ BS.unpack comment
             })
      _ -> Error ""
      {----------------------------------------------------------------------------}
  | otherwise = Error ""

deleteSqlComment1,selectSqlComment :: String -> BS.ByteString
deleteSqlComment1 idOfComment =
  BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++ idOfComment

selectSqlComment id1 =
  BS.pack $ "SELECT * FROM comments WHERE id_of_new =" ++ id1

deleteSqlComment2 :: String -> String -> BS.ByteString
deleteSqlComment2 idOfNew idOfComment =
  BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++
  idOfComment ++ " AND id_of_new = " ++ idOfNew 

insertSqlComment :: BS.ByteString
insertSqlComment =
  BS.pack "INSERT INTO comments(id_of_comment,id_of_user,id_of_new,text_of_comment) VALUES (DEFAULT,$2,$3,$4)"


lookupStuff :: [BS.ByteString] -> Request -> [Maybe (Maybe BS.ByteString)]
lookupStuff [] _ = []
lookupStuff (stuff:s) lst =
  (Prelude.lookup stuff $ queryString lst) : (lookupStuff s lst)


responseOkJSON, responseNotFoundJSON, responseBadRequestJSON ::
     ByteString -> Response
responseOkJSON = responsePlainTextJSON status200

responseNotFoundJSON = responsePlainTextJSON notFound404

responseBadRequestJSON = responsePlainTextJSON badRequest400

responsePlainTextJSON :: Network.HTTP.Types.Status -> ByteString -> Response
responsePlainTextJSON =
  (`responseLBS` [(hContentType, "application/json; charset=utf-8")])
