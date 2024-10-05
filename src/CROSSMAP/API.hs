{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.API
  ( API
  , PublicAPI
  , PrivateAPI
  , SecureUserAPI
  , SecureSessionAPI
  , SessionAPI
  , SessionsAPI
  , PublicKeysAPI
  , UserAPI
  , UsersAPI
  , api
  , loginAPI
  , publicAPI
  , privateAPI
  ) where

import Data.Text (Text)
import Servant

import CROSSMAP.Base64PublicKey
import CROSSMAP.Index
import CROSSMAP.Login
import CROSSMAP.PublicKey
import CROSSMAP.Session
import CROSSMAP.User


type API = PublicAPI :<|> SecureUserAPI :<|> SecureSessionAPI


type PublicAPI = IndexEndpoint


type LoginAPI = "login" :> LoginEndpoint


type PrivateAPI
  = "session" :> SessionAPI
  :<|> "user" :> UserAPI
  :<|> "users" :> UsersAPI
  :<|> "public_keys" :> PublicKeysAPI
  :<|> "sessions" :> SessionsAPI


type SecureUserAPI = AuthProtect "signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "signature" :> PrivateAPI


type SessionAPI = GetSessionEndpoint :<|> DeleteSessionEndpoint


type SessionsAPI
  = ( Get '[JSON] [Base64PublicKey] )
  :<|> ( Capture "session" Base64PublicKey :> SessionAPI )


type PublicKeysAPI
  = ( Get '[JSON] [Base64PublicKey] )
  :<|> ( ReqBody '[JSON] CreatePublicKeyRequest :> Post '[JSON] PublicKeyInfo )
  :<|> ( Capture "public_key" Base64PublicKey :> Get '[JSON] PublicKeyInfo )


type UserAPI = GetUserEndpoint


type UsersAPI
  = ( Get '[JSON] [UserId] )
  :<|> ( ReqBody '[JSON] CreateUserRequest :> Post '[JSON] UserResponse )
  :<|> ( Capture "user" UserId :> UserAPI )
  :<|> ( Capture "username" Text :> UserAPI )


type IndexEndpoint = Get '[JSON] IndexResponse


type LoginEndpoint = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse


type GetSessionEndpoint = Get '[JSON] SessionResponse


type GetUserEndpoint = Get '[JSON] UserResponse


type DeleteSessionEndpoint = Delete '[JSON] NoContent


api :: Proxy API
api = Proxy


loginAPI :: Proxy LoginAPI
loginAPI = Proxy


publicAPI :: Proxy PublicAPI
publicAPI = Proxy


privateAPI :: Proxy PrivateAPI
privateAPI = Proxy
