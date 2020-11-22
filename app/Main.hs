{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import API (statusError)

{----------------------------------------}
import DB
import qualified Hasql.Decoders as HD (text, nonNullable, rowVector, column, nullable, int8, noResult, Result)
import qualified Hasql.Encoders as HE (Params, noParams)
import Logging.Level (LogLevel(DEBUG, INFO))
import Logging.Logging as L (runLog)
import Types

{----------------------------------------}
import Web.JWT as JWT

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy.Char8 as B (pack, unpack)
import Data.ByteString.Lazy.UTF8
import Data.Map.Strict (toList)
import Data.Text as DT (empty, Text, pack, unpack)

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
import Data.Functor.Contravariant ()
import qualified Data.Map as Map

{----------------------------------------}
yoursecret :: BS.ByteString
yoursecret = "SERVER_HASKELL"

-- Main function
main :: IO ()
main = do
  L.runLog INFO "Server is run.."
  run 8000 application

-- Web app
-- Parse of jwt-token and taking user role
application :: Application
application req respond = do
  case pathInfo req of
    ("registration":_) -> routeRegistration  req respond
    ("login":_)        -> routeLogin         req respond
    _ ->  
     case fmap (Prelude.lookup "role") $ jwtCheck (route req) of
     Just (Just role) -> applicationPath (DA.encode role) req respond
     _ -> do
       L.runLog DEBUG $
         "error: problem with token" ++ (show $ jwtCheck (route req))
       respond $
         responseBadRequestJSON $ DA.encode ("error: problem with token" :: Text)
       ----------------------------

--help function for role and pagination
route :: Request -> Text
route req = Prelude.head $ helphead (pathInfo req)

jwtCheck :: Text -> (Maybe [(Text, Value)])
jwtCheck content =
  fmap
    (toList . unClaimsMap . unregisteredClaims . JWT.claims)
    (JWT.decodeAndVerifySignature (HMACSecret yoursecret) content)

helphead [] = ["error: problem with token"]
helphead (x:_) = [x]

applicationPath :: ByteString -> Application
applicationPath role req respond =
  case pathInfo req of
    (_:_:"news":id1:"comments":_) -> routeComments id1 role req respond
    (_:_:"news":_)                -> routeNews     role req respond
    (_:_:"autors":_)              -> routeAutors   role req respond
    (_:_:"drafts":_)              -> routeDrafts   role req respond
    (_:_:"users":"me":_)          -> routeMe       role req respond
    (_:_:"users":_)               -> routeUsers    role req respond
    (_:_:"tags":_)                -> routeTags     role req respond
    (_:_:"categories":_)          -> routeCategory role req respond
    _ ->
      respond $
      responseNotFoundJSON $ encode $ statusError $ Just "error: no Path"

routeRegistration req respond = 
  case requestMethod req of
    "POST" -> case fmap join (lookupStuff ["login", "password"] req) of
              (Just login:Just password:_) -> 
                dbQueryReg
                ((toString . fromStrict) login)
                (Registration
                   { user_id_reg = 0
                   , login = (DT.pack . toString . fromStrict) login
                   , password = (DT.pack . toString . fromStrict) password
                   , token = ""
                   })
                insertSqlRegistr
                someRegistrationEncoder
                req
                respond
              _ -> respond $
                   responseNotFoundJSON $
                   encode $ statusError $ Just "error : no query (registration)" 
    _ -> respond $
         responseNotFoundJSON $
         encode $ statusError $ Just "error : problem with Method (registration)"

dbQueryReg :: ToJSON a => String -> a -> BS.ByteString -> HE.Params a -> Application
dbQueryReg log data1 sql enc req respond = do
  res1 <- connectToDB2 data1 sql enc
  id1  <- connectToDB 1 (BS.pack $ "SELECT user_id FROM registration WHERE login = '" ++ log ++ "'") (HD.rowVector $ Types.Integer1 <$> (HD.column (HD.nonNullable $ fromIntegral <$> HD.int8)))        
  res2 <- connectToDB2  data1 (BS.pack $ "UPDATE registration SET token = '" ++ (DT.unpack $ createToken $ int <$> (getId id1)) ++ "' WHERE user_id = " ++ (fromMaybe $ getId id1)) enc        
  print "-------------------"
  print log
  print "-------------------"
  print id1
  print "-------------------"
  print res1
  (respond $
   responseOkJSON
     (case rawQueryString req of
        _ -> encode res1))
  where getId id1          = fmap DV.head $ result id1
        fromMaybe Nothing  = "NULL"
        fromMaybe (Just x) = show $ int x 
        createToken (Just x) = let
                        cs = mempty { 
                        unregisteredClaims = ClaimsMap $ Map.fromList [(DT.pack "user_id",toJSON x),(DT.pack "role",String "User")]
                        }
                        key = hmacSecret $ DT.pack $ BS.unpack yoursecret
                        in encodeSigned key mempty cs
        createToken Nothing = DT.empty

insertSqlRegistr :: BS.ByteString
insertSqlRegistr = BS.pack "INSERT INTO registration(user_id,login,password,token) VALUES(DEFAULT,$2,$3,'')"

routeLogin ::  Application
routeLogin req respond = 
  case queryString req of
    [("login",Just login),("password",Just password)] -> dbQuery (HD.rowVector $ Types.Text1 <$> (HD.column (HD.nonNullable HD.text))) 
                                               (BS.pack $ "SELECT token FROM registration WHERE login = '" ++ BS.unpack login ++ "' AND password = '" ++ BS.unpack password ++ "';") req respond
    _ -> respond $
         responseNotFoundJSON $
         encode $ statusError $ Just "error : no query (Login)"

routeNews :: ByteString -> Application
routeNews _ req respond =
  case pathInfo req of
    [_,_, "news"] ->
      dbQuery
        someNewsDecoder
        (BS.pack $ selectSqlNews ++ " ORDER BY nn.id_of_new DESC")
        req
        respond
    [_,_, "news", "filter"] ->
      case queryString req of
        ("date_of_create_at_gt", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             "AND nn.date_of_create > '" ++ BS.unpack x ++ "' ;")
            req
            respond
        ("date_of_create_at_lt", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             "AND nn.date_of_create < '" ++ BS.unpack x ++ "' ;")
            req
            respond
        ("name_of_autor", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             "AND ((nn.a).u).first_name = '" ++ BS.unpack x ++ "' ;")
            req
            respond
        ("category", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             "AND (nn.categories).category_id = '" ++ BS.unpack x ++ "'  ;")
            req
            respond
        ("tag", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             "SELECT * FROM nestednews nn, unnest(nn.tags) as unntag WHERE unntag.tag_id=" ++
             BS.unpack x)
            req
            respond
        ("tag_in", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             "SELECT * FROM nestednews nn, unnest(nn.tags) as unntag WHERE unntag.tag_id = ANY(ARRAY" ++
             BS.unpack x ++ ");")
            req
            respond
        ("tag_all", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             "SELECT * FROM nestednews nn, unnest(nn.tags) as unntag WHERE unntag.tag_id = ALL(ARRAY" ++
             BS.unpack x ++ "::integer[]);")
            req
            respond
        ("name", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             "AND (nn.name) LIKE ('%" ++ BS.unpack x ++ "%'::text)  ;")
            req
            respond
        ("text_of_new", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             "AND (nn.text_of_new) LIKE ('%" ++ BS.unpack x ++ "%'::text) ;")
            req
            respond
        ("id_of_new", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++ "AND nn.id_of_new= " ++ BS.unpack x ++ " ;")
            req
            respond
        _ ->
          respond $
          responseNotFoundJSON $
          encode $ statusError $ Just "error : no query filter (news)"
    {----------------------------------------------------------------}
    [_,_, "news", "order"] ->
      case queryString req of
        ("date_of_create", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             " ORDER BY nn.date_of_create " ++ BS.unpack x ++ ";")
            req
            respond
        ("name_of_autor", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             " ORDER BY ((nn.a).u).first_name " ++ BS.unpack x ++ ";")
            req
            respond
        ("category", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             selectSqlNews ++
             " ORDER BY (nn.categories).category_name " ++ BS.unpack x ++ ";")
            req
            respond
        ("amount_of_photo", Just "DESC"):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $ selectSqlNews ++ " ORDER BY array_length(images,2);")
            req
            respond
        ("amount_of_photo", Just "ASC"):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $ selectSqlNews ++ " ORDER BY array_length(images,1);")
            req
            respond
        _ ->
          respond $
          responseNotFoundJSON $
          encode $ statusError $ Just "error : no query order (news)"
    {----------------------------------------------------------------}
    [_,_, "news", "search"] ->
      case queryString req of
        ("content", Just x):_ ->
          dbQuery
            someNewsDecoder
            (BS.pack $
             "SELECT * FROM nestedNewsSearch('%" ++ BS.unpack x ++ "%');")
            req
            respond
        _ ->
          respond $
          responseNotFoundJSON $
          encode $ statusError $ Just "error : no query search (news)"
    {----------------------------------------------------------------}
    _ ->
      respond $
      responseNotFoundJSON $
      encode $ statusError $ Just "error: path or query news"

selectSqlNews :: String
selectSqlNews = "SELECT * FROM nestednews nn WHERE true "

routeAutors :: ByteString -> Application
routeAutors role req respond =
  case role of
    "\"Admin\"" ->
      case requestMethod req of
        "GET" ->
          dbQuery
            someAutorsDecoderNotNested
            (BS.pack "SELECT * FROM nestedautor;")
            req
            respond
          {----------------------------------------------------------------}
        "POST" ->
          case fmap join (lookupStuff ["user_id", "description"] req) of
            [Just x, y] ->
              dbQueryInsert
                (AutorsEncoder
                   { user_id_autors_en = read (BS.unpack x) :: Integer
                   , description_en = fmap (DT.pack . toString . fromStrict) y
                   })
                insertsqlAutor
                someAutorsEncoder
                req
                respond
            _ ->
              respond $
              responseNotFoundJSON $
              encode $
              statusError $ Just "error: Problem with queryString (autors) POST"
          {----------------------------------------------------------------}
        "DELETE" ->
          case queryString req of
            ([("user_id", Just x)]) ->
              dbQueryInsert
                (AutorsEncoder
                   { user_id_autors_en = (read . toString . fromStrict) x
                   , description_en = Nothing
                   })
                deletesqlAutor
                someAutorsEncoder
                req
                respond
            _ ->
              respond $
              responseNotFoundJSON $
              encode $
              statusError $
              Just "error: Problem with queryString (autors) DELETE"
        "PUT" ->
          case queryString req of
            ([("description", x), ("user_id", Just desc)]) ->
              dbQueryInsert
                (AutorsEncoder
                   { user_id_autors_en = (read . toString . fromStrict) desc
                   , description_en = fmap (DT.pack . toString . fromStrict) x
                   })
                putAutors
                someAutorsEncoder
                req
                respond
            _ ->
              respond $
              responseNotFoundJSON $
              encode $
              statusError $
              Just "error: Problem with queryString (autors) PUT"
        _ ->
          respond $
          responseNotFoundJSON $
          encode $
          statusError $ Just "error: Problem with rqstMethod (autors) DELETE"
  {------------------------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ B.pack $ "role is " ++ (B.unpack role)

putAutors,insertsqlAutor,deletesqlAutor :: BS.ByteString
putAutors = BS.pack "UPDATE autors SET description = $1 WHERE user_id=$2;"

insertsqlAutor = BS.pack "INSERT INTO autors(user_id, description) VALUES ($2,$1);"

deletesqlAutor = BS.pack "DELETE FROM autors WHERE user_id = $2;"

routeCategory :: ByteString -> Application
routeCategory role req respond
  | requestMethod req == "GET" =
    dbQuery
      someCategoriesDecoderNotNested
      ("SELECT * FROM categories")
      req
      respond
  | role == "\"Admin\"" =
    case requestMethod req of
      "POST" ->
        case fmap join (lookupStuff ["parent_id", "category_name"] req) of
          (catId:Just catName:_) ->
            dbQueryInsert
              (Category
                 { parent_id =
                     fmap
                       ((\x -> read x :: Integer) . toString . fromStrict)
                       catId
                 , category_name = DT.pack $ toString $ fromStrict $ catName
                 , category_id = 0
                 })
              "INSERT INTO categories(child_id,category_name,category_id) VALUES ($1,$2,DEFAULT);"
              someCategoriesEncoder
              req
              respond
          _ ->
            respond $
            responseNotFoundJSON $
            encode $ statusError $ Just "Problem with no query (category) POST"
      "DELETE" ->
        case join $ Prelude.lookup "category_id" (queryString req) of
          (Just category) ->
            dbQueryInsert
              ()
              (BS.pack $
               "DELETE FROM categories WHERE category_id = " ++ BS.unpack category)
              HE.noParams
              req
              respond
          _ ->
            respond $
            responseNotFoundJSON $
            encode $ statusError $ Just "Problem with no query (users) DELETE"
      "PUT" ->
        case fmap join (lookupStuff ["category_id", "category_name","parent_id"] req) of
          (Just categoryId:Just categoryText:prntId:_) ->
            dbQueryInsert
              (Category
                 { parent_id = fmap ((\x -> read x :: Integer) . toString . fromStrict) prntId
                 , category_name =
                     DT.pack $ toString $ fromStrict $ categoryText
                 , category_id =
                     ((\x -> read x :: Integer) . toString . fromStrict)
                       categoryId
                 })
              ("UPDATE categories SET category_name = $2, child_id = $1 WHERE category_id=$3;")
              (someCategoriesEncoder)
              req
              respond
          _ ->
            respond $
            responseNotFoundJSON $
            encode $ statusError $ Just "Problem with no query (category) PUT"
      _ -> respond $ responseNotFoundJSON $  "Problem with reqMethod (category)"
  | otherwise =
    respond $
    responseNotFoundJSON $
    encode $ statusError $ Just "Problem with role or rqstMethod (category)"

routeTags :: ByteString -> Application
routeTags role req respond
    | requestMethod req == "GET" =
        dbQuery
        (someTagsDecoderNotNested)
        ("SELECT * FROM tags")
        req
        respond
    | role == "\"Admin\"" =
      case requestMethod req of 
        "POST" ->
            case fmap join (lookupStuff ["text_of_tag"] req) of
              [Just tag] ->
                dbQueryInsert
                (Tags {tag_name = (DT.pack . BS.unpack) tag, tag_id = 0})
                ("INSERT INTO tags(tag_name,tag_id) VALUES ($1,DEFAULT);")
                (someTagsEncoderNotNested)
                req
                respond
              _ ->
                respond $
                responseNotFoundJSON $
                encode $ statusError $ Just "Problem with no query (tags) POST"
        "DELETE" ->
            case join $ Prelude.lookup "tag_id" (queryString req) of
              (Just tag) ->
                dbQueryInsert
                ()
                (BS.pack $ "DELETE FROM users WHERE user_id = " ++ BS.unpack tag)
                HE.noParams
                req
                respond
              _ ->
                respond $
                responseNotFoundJSON $
                encode $ statusError $ Just $ "Problem with no query (tags) DELETE"
        "PUT" ->
          case fmap join (lookupStuff ["tag_id", "text_of_tag"] req) of
            (Just tagId:Just tagText:_) ->
              dbQueryInsert
              (Tags
              { tag_name = DT.pack $ toString $ fromStrict $ tagText
              , tag_id =((\x -> read x :: Integer) . toString . fromStrict) tagId
              })
              ("UPDATE tags SET tag_name = $1 WHERE tag_id=$2;")
              (someTagsEncoderNotNested)
              req
              respond
            _ ->
              respond $
              responseNotFoundJSON $
              encode $ statusError $ Just "Problem with no query (tags) DELETE"
        _ -> respond $ responseNotFoundJSON $ role
    | otherwise =
      respond $
      responseNotFoundJSON $
      encode $ statusError $ Just "Problem with reqMethod or query (tags)"

routeDrafts :: ByteString -> Application
routeDrafts role req respond =
  case role of
    "\"Autor\"" ->
      case pathInfo req of
                --WARNING
        (_:"drafts":"publish":_) ->
          case (Prelude.lookup "id_of_draft" (queryString req)) of
            (Just x) -> dbQueryInsert () insertsqlNews HE.noParams req respond
            _ ->
              respond $
              responseNotFoundJSON $
              encode $
              statusError $
              Just "error: Problem with queryString (drafts) publish POST"
                {---------------------------------------------}
        _ ->
          case requestMethod req of
            "GET" ->
              dbQuery
                someDraftsDecoder
                (selectsqlDraft req)
                req
                respond
                          {-----------------------------------------------------------------}
            "POST" ->
              case fmap join (lookupStuff ["text_of_draft"] req) of
                ([(Just z)]) ->
                  dbQueryInsert
                    (Draft
                       { id_of_draft = 0
                       , id_of_user = read $ getId req
                       , text_of_draft = DT.pack (toString $ fromStrict z)
                       })
                    (insertsqlDraft)
                    someDraftsEncoder
                    req
                    respond
                _ ->
                  respond $
                  responseNotFoundJSON $
                  encode $
                  statusError $
                  Just "error: Problem with queryString (drafts) POST"
                          {----------------------------------------------------------------------}
            "PUT" ->
              case fmap join (lookupStuff ["id_of_draft", "text_of_draft"] req) of
                ([(Just x), (Just z)]) ->
                  dbQueryInsert
                    (Draft
                       { id_of_draft = read (toString $ fromStrict x)
                       , id_of_user = read $ getId req
                       , text_of_draft = DT.pack (toString $ fromStrict z)
                       })
                    (updateqlDraft)
                    someDraftsEncoder
                    req
                    respond
                _ ->
                  respond $
                  responseNotFoundJSON $
                  encode $
                  statusError $
                  Just "error: Problem with queryString (drafts) PUT"
                          {----------------------------------------------------------------------}
            "DELETE" ->
              case fmap join (lookupStuff ["id_of_draft"] req) of
                ([(Just x)]) ->
                  dbQueryInsert () (deleteSqlDraft req) HE.noParams req respond
                _ ->
                  respond $
                  responseNotFoundJSON $
                  encode $
                  statusError $
                  Just "error: Problem with queryString (drafts) DELETE"
          {------------------------------------------}
    _ -> respond $ responseNotFoundJSON $ role

{-query for drafts-}
insertsqlDraft,updateqlDraft,insertsqlNews :: BS.ByteString
insertsqlDraft =
  BS.pack
    "INSERT INTO drafts(id_of_draft,user_id,text_of_draft) VALUES (DEFAULT,$2,$3);"

insertsqlNews =
  BS.pack "UPDATE news n SET text_of_new = d.text_of_draft FROM drafts d WHERE n.id_of_new=2 and d.id_of_draft=4 ; "

updateqlDraft =
  BS.pack "UPDATE drafts SET text_of_draft = $3 WHERE id_of_draft=$1;"

deleteSqlDraft,selectsqlDraft :: Request -> BS.ByteString
deleteSqlDraft req =
  BS.pack $ "DELETE FROM drafts WHERE id_of_draft = " ++ (getId req) ++ ";"

selectsqlDraft req =
  BS.pack $ "SELECT * FROM drafts WHERE user_id=" ++ (getId req) ++ ";"

{-------------------------------}

routeMe :: ByteString -> Application
routeMe role req respond 
  | requestMethod req == "GET" =
        dbQuery
          someUsersDecoderNotNested
          (selectSqlUserid $ getId req)
          req
          respond
  | otherwise =
    respond $
    responseNotFoundJSON $
    encode $ statusError $ Just "Problem with reqMethod (Me)"

routeUsers :: ByteString -> Application
routeUsers role req respond
  | requestMethod req == "GET" =
        dbQuery
          (someUsersDecoderNotNested)
          (selectSqlUser)
          req
          respond
  | requestMethod req == "PUT" =
    case fmap join (lookupStuff ["first_name", "second_name", "image"] req) of
      [first, second, image] ->
        dbQueryInsert
          (Users
             { image = fmap (DT.pack . BS.unpack) image
             , date_of_create_user = fromGregorian 0 0 0
             , user_id = read (getId req) :: Integer
             , first_name = fmap (DT.pack . BS.unpack) first
             , second_name = fmap (DT.pack . BS.unpack) second
             })
          updateSqlUser
          someUsersEncoder
          req
          respond
      _ ->
        respond $
        responseNotFoundJSON $
        encode $ statusError $ Just "Problem with no query (users) PUT"
  | requestMethod req == "DELETE" =
    case role of
      "\"Admin\"" ->
        case join $ Prelude.lookup "user_id" (queryString req) of
          (Just user) ->
            dbQueryInsert () (deleteSqlUser $ BS.unpack user) HE.noParams req respond
          _ ->
            respond $
            responseNotFoundJSON $
            encode $ statusError $ Just "Problem with no query (users) DELETE"
      _ -> respond $ responseNotFoundJSON $ role
  | otherwise =
    respond $
    responseNotFoundJSON $
    encode $ statusError $ Just "Problem with reqMethod (users)"

selectSqlUser,insertsqlUsers,updateSqlUser :: BS.ByteString
selectSqlUser = BS.pack "SELECT * FROM users;"

insertsqlUsers = "INSERT INTO users(image,date_of_create,user_id,first_name,second_name) VALUES ($1,DEFAULT,DEFAULT,$4,$5)"

updateSqlUser = "UPDATE users SET first_name = $4, second_name=$5, image =$1 WHERE user_id=$3;"

selectSqlUserid,deleteSqlUser :: String -> BS.ByteString
selectSqlUserid x = BS.pack $ "SELECT * FROM users WHERE user_id = " ++ x ++ ";"

deleteSqlUser user =
  BS.pack $ "DELETE FROM registration WHERE user_id = " ++ user ++ ";"


routeComments :: Text -> ByteString -> Application
routeComments idNew role req respond
  | requestMethod req == "GET" =
    dbQuery
      someCommentsDecoderNotNested
      (selectSqlComment $ DT.unpack idNew)
      req
      respond
      {----------------------------------------------------------------------------}
  | requestMethod req == "DELETE" =
    case join (Prelude.lookup "id_of_comment" (queryString req)) of
      (Just idComment) ->
        case role of
          "\"Admin\"" ->
            dbQueryInsert
              ()
              (deleteSqlComment1 $ BS.unpack idComment)
              HE.noParams
              req
              respond
          "\"Autor\"" ->
            dbQuery
              someCommentsDecoderNotNested
              (deleteSqlComment2 (BS.unpack idComment) $ getId req)
              req
              respond
          _ ->
            respond $
            responseNotFoundJSON $
            encode $
            statusError $ Just "error: Problem with role (comments) DELETE"
      _ ->
        respond $
        responseNotFoundJSON $
        encode $
        statusError $ Just "error: Problem with queryString (comments) DELETE"
      {----------------------------------------------------------------------------}
  | requestMethod req == "POST" =
    case join (Prelude.lookup "text_of_comment" (queryString req)) of
      (Just comment) ->
        dbQueryInsert
          (CommentsNotNested
             { id_of_comment_nt = 0
             , user_id_comment_nt = read $ getId req
             , id_of_new_comment_nt = read $ DT.unpack idNew
             , text_of_comment_nt = DT.pack $ BS.unpack comment
             })
          (insertSqlComment)
          someCommentsEncoderNotNested
          req
          respond
      _ ->
        respond $
        responseNotFoundJSON $
        encode $
        statusError $ Just "error: Problem with queryString (comments) POST"
      {----------------------------------------------------------------------------}
  | otherwise =
    respond $
    responseNotFoundJSON $
    encode $ statusError $ Just "error: Problem with rqstMethod (comments)"

deleteSqlComment1,selectSqlComment :: String -> BS.ByteString
deleteSqlComment1 idOfComment =
  BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++ idOfComment

selectSqlComment id1 =
  BS.pack $ "SELECT * FROM comments WHERE id_of_new =" ++ id1 ++ " ;"

deleteSqlComment2 :: String -> String -> BS.ByteString
deleteSqlComment2 idOfNew idOfComment =
  BS.pack $ "DELETE FROM comments WHERE id_of_comment = " ++
  idOfComment ++ " AND id_of_new = " ++ idOfNew ++ ";"

insertSqlComment :: BS.ByteString
insertSqlComment =
  BS.pack "INSERT INTO comments(id_of_comment,id_of_user,id_of_new,text_of_comment) VALUES (DEFAULT,$2,$3,$4)"

getId req = fromMaybe (fmap (Prelude.lookup "user_id") $ jwtCheck (route req))

fromMaybe :: ToJSON a => Maybe (Maybe a) -> String
fromMaybe (Just (Just x)) = toString $ DA.encode x
fromMaybe _               = ""

lookupStuff :: [BS.ByteString] -> Request -> [Maybe (Maybe BS.ByteString)]
lookupStuff [] _ = []
lookupStuff (stuff:s) lst =
  (Prelude.lookup stuff $ queryString lst) : (lookupStuff s lst)


dbQuery :: ToJSON a =>
     HD.Result (Vector a)
  -> BS.ByteString
  -> Application
dbQuery dec sql req respond = do
  res <- connectToDB 2 sql dec
  (respond $
   responseOkJSON
     (case res of
        _ -> encode res))
        
pagin :: Request -> Int
pagin req = read (DT.unpack $ last $ Prelude.take 2 $ helphead (pathInfo req)) :: Int

dbQueryInsert :: ToJSON a => a -> BS.ByteString -> HE.Params a -> Application
dbQueryInsert data1 sql enc req respond = do
  res <- connectToDB2 data1 sql enc
  (respond $
   responseOkJSON
     (case rawQueryString req of
        _ -> encode res))

dbQueryPOSTUser :: ToJSON a => a -> BS.ByteString -> HE.Params a -> Application
dbQueryPOSTUser data1 sql enc req respond = do
  res <- connectToDB2 data1 sql enc
  (respond $
   responseOkJSON
     (case rawQueryString req of
        _ -> encode res))


responseOkJSON, responseNotFoundJSON, responseBadRequestJSON ::
     ByteString -> Response
responseOkJSON = responsePlainTextJSON status200

responseNotFoundJSON = responsePlainTextJSON notFound404

responseBadRequestJSON = responsePlainTextJSON badRequest400

responsePlainTextJSON :: Network.HTTP.Types.Status -> ByteString -> Response
responsePlainTextJSON =
  (`responseLBS` [(hContentType, "application/json; charset=utf-8")])
