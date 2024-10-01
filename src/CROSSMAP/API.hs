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
  , api
  , loginAPI
  , publicAPI
  , privateAPI
  ) where

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


type SecureUserAPI = AuthProtect "signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "signature" :> PrivateAPI


type SessionAPI = GetSessionEndpoint :<|> DeleteSessionEndpoint


type UserAPI = GetUserEndpoint


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
