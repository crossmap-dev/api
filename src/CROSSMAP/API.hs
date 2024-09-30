{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.API
  ( API
  , PublicAPI
  , PrivateAPI
  , SecureUserAPI
  , SecureSessionAPI
  , api
  , loginAPI
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


type PrivateAPI
  = "session" :> SessionAPI


type SecureUserAPI = AuthProtect "signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "signature" :> PrivateAPI


type SessionAPI = GetSessionEndpoint :<|> DeleteSessionEndpoint


type IndexEndpoint = Get '[JSON] IndexResponse


type LoginEndpoint = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse


type GetSessionEndpoint = Get '[JSON] SessionResponse


type DeleteSessionEndpoint = Delete '[JSON] NoContent


api :: Proxy API
api = Proxy


loginAPI :: Proxy LoginAPI
loginAPI = Proxy


publicAPI :: Proxy PublicAPI
publicAPI = Proxy


privateAPI :: Proxy PrivateAPI
privateAPI = Proxy
