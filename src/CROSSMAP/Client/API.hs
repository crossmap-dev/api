{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.Client.API
  ( SessionClient(..)
  , PublicKeyClient(..)
  , loginClient
  , getSessionClient
  , deleteSessionClient
  , getPublicKeyClientByPublicKey
  , getPublicKeysClient
  , getUserClient
  , getUsersClient
  , createUserClient
  , createPublicKeyClient
  , getUserByIdClient
  , getUserByUsernameClient
  , getSessionsClient
  , getSessionClientByPublicKey
  ) where

import Data.Text (Text)
import Servant
import Servant.Client

import CROSSMAP.API
import CROSSMAP.Base64PublicKey
import CROSSMAP.Login
import CROSSMAP.PublicKey
import CROSSMAP.Session
import CROSSMAP.User


type LoginClientM = LoginRequest -> ClientM LoginResponse


type SessionClientM = GetSessionClientM :<|> DeleteSessionClientM


type SessionsClientM = GetSessionsClientM :<|> (Base64PublicKey -> SessionClientM)


type PublicKeysClientM
  = GetPublicKeysClientM
  :<|> CreatePublicKeyClientM
  :<|> (Base64PublicKey -> PublicKeyClientM)


type GetPublicKeysClientM = ClientM [Base64PublicKey]


type CreatePublicKeyClientM = CreatePublicKeyRequest -> ClientM PublicKeyInfo


type PublicKeyClientM = GetPublicKeyClientM :<|> DeletePublicKeyClientM


type GetPublicKeyClientM = ClientM PublicKeyInfo


type DeletePublicKeyClientM = ClientM NoContent


type UserClientM = GetUserClientM


type UsersClientM
  = GetUsersClientM
  :<|> CreateUserClientM
  :<|> GetUserByIdClientM
  :<|> GetUserByUsernameClientM


type GetSessionClientM = ClientM SessionResponse


type DeleteSessionClientM = ClientM NoContent


type GetUserClientM = ClientM UserResponse


type GetUsersClientM = ClientM [UserId]


type CreateUserClientM = CreateUserRequest -> ClientM UserResponse


type GetUserByIdClientM = UserId -> GetUserClientM


type GetUserByUsernameClientM = Text -> GetUserClientM


type GetSessionsClientM = ClientM [Base64PublicKey]


data PublicKeyClient = PublicKeyClient
  { getPublicKey :: ClientM PublicKeyInfo
  , deletePublicKey :: ClientM NoContent
  }


data SessionClient = SessionClient
  { getSession :: GetSessionClientM
  , deleteSession :: DeleteSessionClientM
  }


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
userClient :: UserClientM
usersClient :: UsersClientM
publicKeysClient :: PublicKeysClientM
sessionsClient :: SessionsClientM
publicKeysClient
  :<|> sessionClient
  :<|> sessionsClient
  :<|> userClient
  :<|> usersClient
  = client privateAPI


getSessionClient :: GetSessionClientM
deleteSessionClient :: DeleteSessionClientM
getSessionClient :<|> deleteSessionClient = sessionClient


getUserClient :: GetUserClientM
getUserClient = userClient


getUsersClient :: GetUsersClientM
createUserClient :: CreateUserClientM
getUserByIdClient :: GetUserByIdClientM
getUserByUsernameClient :: GetUserByUsernameClientM
getUsersClient
  :<|> createUserClient
  :<|> getUserByIdClient
  :<|> getUserByUsernameClient
  = usersClient


getPublicKeysClient :: GetPublicKeysClientM
createPublicKeyClient :: CreatePublicKeyClientM
getPublicKeyClient :: Base64PublicKey -> PublicKeyClientM
getPublicKeysClient :<|> createPublicKeyClient :<|> getPublicKeyClient = publicKeysClient


getPublicKeyClientByPublicKey :: Base64PublicKey -> PublicKeyClient
getPublicKeyClientByPublicKey pk =
  let getPublicKey' :<|> deletePublicKey' = getPublicKeyClient pk
   in PublicKeyClient
        { getPublicKey = getPublicKey'
        , deletePublicKey = deletePublicKey'
        }


getSessionsClient :: GetSessionsClientM
sessionClientByPublicKey :: Base64PublicKey -> SessionClientM
getSessionsClient :<|> sessionClientByPublicKey = sessionsClient


getSessionClientByPublicKey :: Base64PublicKey -> SessionClient
getSessionClientByPublicKey pk =
  let getSession' :<|> deleteSession' = sessionClientByPublicKey pk
   in SessionClient { getSession = getSession', deleteSession = deleteSession' }
