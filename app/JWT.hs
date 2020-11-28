{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module JWT where

import Web.JWT as JWT
    ( claims,
      decodeAndVerifySignature,
      encodeSigned,
      hmacSecret,
      ClaimsMap(ClaimsMap, unClaimsMap),
      JWTClaimsSet(unregisteredClaims),
      Signer(HMACSecret) )
import Data.Aeson as DA ( encode, Value )
import Data.Text ( Text )
import Data.Aeson ()
import qualified Data.ByteString.Char8 as BS
import Network.Wai
  ( Request
  , pathInfo
  , rawQueryString
  , requestMethod
  )
import Data.Map.Strict (toList)
import Data.ByteString.Lazy.UTF8 (toString, ByteString )
import Data.Aeson.Types ( ToJSON )


yoursecret :: BS.ByteString
yoursecret = "SERVER_HASKELL"
-- Parse of jwt-token and taking user role
takeRoleJWT :: Request -> ByteString
takeRoleJWT req =
  if Prelude.length (pathInfo req) > 2 
  then  
    case Prelude.lookup "role" <$> jwtCheck (route req) of
    Just (Just role) -> DA.encode role
    _ -> DA.encode (show $ jwtCheck (route req))
  else ""     

--help function for role and pagination
route :: Request -> Text
route req = Prelude.head $ helphead (pathInfo req)
       where helphead [] = ["error: problem with token"]
             helphead (x:_) = [x]

jwtCheck :: Text -> Maybe [(Text, Value)]
jwtCheck content =
  fmap (toList . unClaimsMap . unregisteredClaims . JWT.claims)
  (JWT.decodeAndVerifySignature (HMACSecret yoursecret) content)

getId req = fromMaybe (fmap (Prelude.lookup "user_id") $ jwtCheck (route req))

fromMaybe :: ToJSON a => Maybe (Maybe a) -> String
fromMaybe (Just (Just x)) = toString $ DA.encode x
fromMaybe _               = ""