{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.API
  ( API
  , PublicAPI
  , PrivateAPI
  , SecureUserAPI
  , SecureSessionAPI
  , PolicyAPI
  , PoliciesAPI
  , PublicKeyAPI
  , PublicKeysAPI
  , SessionAPI
  , SessionsAPI
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
import CROSSMAP.Policy
import CROSSMAP.PublicKey
import CROSSMAP.Session
import CROSSMAP.User


type API = PublicAPI :<|> SecureUserAPI :<|> SecureSessionAPI


type PublicAPI = IndexEndpoint


type LoginAPI = "login" :> LoginEndpoint


type PrivateAPI
  = "policies" :> PoliciesAPI
  :<|> "public-keys" :> PublicKeysAPI
  :<|> "session" :> SessionAPI
  :<|> "sessions" :> SessionsAPI
  :<|> "user" :> UserAPI
  :<|> "users" :> UsersAPI


type SecureUserAPI = AuthProtect "signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "signature" :> PrivateAPI


type PolicyAPI = GetPolicyEndpoint :<|> DeletePolicyEndpoint


type PoliciesAPI
  = ( Get '[JSON] [PolicyId] )
  :<|> ( ReqBody '[JSON] CreatePolicyRequest :> Post '[JSON] Policy )
  :<|> ( Capture "policy" PolicyId :> PolicyAPI )


type PublicKeysAPI
  = ( Get '[JSON] [Base64PublicKey] )
  :<|> ( ReqBody '[JSON] CreatePublicKeyRequest :> Post '[JSON] PublicKeyInfo )
  :<|> ( Capture "public-key" Base64PublicKey :> PublicKeyAPI )


type PublicKeyAPI
  = ( Get '[JSON] PublicKeyInfo )
  :<|> ( Delete '[JSON] NoContent )


type SessionAPI = GetSessionEndpoint :<|> DeleteSessionEndpoint


type SessionsAPI
  = ( Get '[JSON] [Base64PublicKey] )
  :<|> ( Capture "session" Base64PublicKey :> SessionAPI )


type UserAPI = GetUserEndpoint


type UsersAPI
  = ( Get '[JSON] [UserId] )
  :<|> ( ReqBody '[JSON] CreateUserRequest :> Post '[JSON] UserResponse )
  :<|> ( Capture "user" UserId :> UserAPI )
  :<|> ( Capture "username" Text :> UserAPI )


type IndexEndpoint = Get '[JSON] IndexResponse


type LoginEndpoint = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse


type GetPolicyEndpoint = Get '[JSON] Policy


type DeletePolicyEndpoint = Delete '[JSON] NoContent


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
