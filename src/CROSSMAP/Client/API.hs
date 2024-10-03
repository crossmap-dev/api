{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.Client.API
  ( SessionClient(..)
  , loginClient
  , getSessionClient
  , deleteSessionClient
  , getUserClient
  , getUsersClient
  , createUserClient
  , getUserByIdClient
  , getUserByUsernameClient
  , getSessionsClient
  , getSessionClientByPublicKey
  ) where

import Data.Text (Text)
import Servant
import Servant.Client

import CROSSMAP.API
import CROSSMAP.Login
import CROSSMAP.PublicKey
import CROSSMAP.Session
import CROSSMAP.User


type LoginClientM = LoginRequest -> ClientM LoginResponse


type SessionClientM = GetSessionClientM :<|> DeleteSessionClientM


type SessionsClientM = GetSessionsClientM :<|> (Base64PublicKey -> SessionClientM)


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


data SessionClient = SessionClient
  { getSession :: GetSessionClientM
  , deleteSession :: DeleteSessionClientM
  }


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
userClient :: UserClientM
usersClient :: UsersClientM
sessionsClient :: SessionsClientM
sessionClient :<|> userClient :<|> usersClient :<|> sessionsClient = client privateAPI


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


getSessionsClient :: GetSessionsClientM
sessionClientByPublicKey :: Base64PublicKey -> SessionClientM
getSessionsClient :<|> sessionClientByPublicKey = sessionsClient


getSessionClientByPublicKey :: Base64PublicKey -> SessionClient
getSessionClientByPublicKey pk =
  let getSession' :<|> deleteSession' = sessionClientByPublicKey pk
   in SessionClient { getSession = getSession', deleteSession = deleteSession' }
