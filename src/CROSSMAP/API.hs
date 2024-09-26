{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.API
  ( API
  , PublicAPI
  , PrivateAPI
  , IndexResponse(..)
  , SessionResponse(..)
  , api
  , publicAPI
  , privateAPI
  ) where

import Servant

import CROSSMAP.Index
import CROSSMAP.Login
import CROSSMAP.Session


type API = PublicAPI :<|> SecureUserAPI :<|> SecureSessionAPI


type PublicAPI = IndexEndpoint


type LoginAPI = "login" :> LoginEndpoint


type PrivateAPI = "session" :> SessionEndpoint


type SecureUserAPI = AuthProtect "user-signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "session-signature" :> PrivateAPI


type IndexEndpoint = Get '[JSON] IndexResponse


type LoginEndpoint = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse


type SessionEndpoint = Get '[JSON] SessionResponse


api :: Proxy API
api = Proxy


publicAPI :: Proxy PublicAPI
publicAPI = Proxy


privateAPI :: Proxy PrivateAPI
privateAPI = Proxy
