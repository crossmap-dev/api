{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.API
  ( API
  , PublicAPI
  , PrivateAPI
  , SecureUserAPI
  , SecureSessionAPI
  , SessionAPI
  , UserAPI
  , UsersAPI
  , api
  , loginAPI
  , publicAPI
  , privateAPI
  ) where

import Data.Text (Text)
import Servant

import CROSSMAP.Index
import CROSSMAP.Login
import CROSSMAP.Session
import CROSSMAP.User


type API = PublicAPI :<|> SecureUserAPI :<|> SecureSessionAPI


type PublicAPI = IndexEndpoint


type LoginAPI = "login" :> LoginEndpoint


type PrivateAPI
  = "session" :> SessionAPI
  :<|> "user" :> UserAPI
  :<|> "users" :> UsersAPI


type SecureUserAPI = AuthProtect "signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "signature" :> PrivateAPI


type SessionAPI = GetSessionEndpoint :<|> DeleteSessionEndpoint


type UserAPI = GetUserEndpoint


type UsersAPI
  = Get '[JSON] [UserId]
  :<|> Capture "user" UserId :> UserAPI
  :<|> Capture "username" Text :> UserAPI


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
