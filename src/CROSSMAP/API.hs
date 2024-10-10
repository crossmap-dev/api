{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.API
  ( module CROSSMAP.API
  ) where

import Data.Text (Text)
import Servant

import CROSSMAP.Base64PublicKey
import CROSSMAP.Group
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
  = "groups" :> GroupsAPI
  :<|> "group-policies" :> GroupPoliciesAPI
  :<|> "policies" :> PoliciesAPI
  :<|> "public-keys" :> PublicKeysAPI
  :<|> "session" :> SessionAPI
  :<|> "sessions" :> SessionsAPI
  :<|> "user" :> UserAPI
  :<|> "users" :> UsersAPI
  :<|> "users-policies" :> UsersPoliciesAPI


type SecureUserAPI = AuthProtect "signature" :> LoginAPI


type SecureSessionAPI = AuthProtect "signature" :> PrivateAPI


type GroupAPI = GetGroupEndpoint :<|> DeleteGroupEndpoint


type GroupsAPI
  = ( Get '[JSON] [GroupId] )
  :<|> ( ReqBody '[JSON] CreateGroupRequest :> Post '[JSON] Group )
  :<|> ( Capture "group" GroupId :> GroupAPI )


type GroupPoliciesAPI
  = ( Capture "group" GroupId :> Get '[JSON] [PolicyId] )
  :<|> ( Capture "group" GroupId :> Capture "policy" PolicyId :> Put '[JSON] NoContent )
  :<|> ( Capture "group" GroupId :> Capture "policy" PolicyId :> Delete '[JSON] NoContent )


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


type UsersPoliciesAPI
  = ( Capture "user" UserId :> Get '[JSON] [PolicyId] )
  :<|> ( Capture "user" UserId :> Capture "policy" PolicyId :> Put '[JSON] NoContent )
  :<|> ( Capture "user" UserId :> Capture "policy" PolicyId :> Delete '[JSON] NoContent )


type IndexEndpoint = Get '[JSON] IndexResponse


type LoginEndpoint = ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse


type GetGroupEndpoint = Get '[JSON] Group


type DeleteGroupEndpoint = Delete '[JSON] NoContent


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
