{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.Client.API
  ( loginClient
  , getSessionClient
  , deleteSessionClient
  , getUserClient
  , getUsersClient
  , getUserByIdClient
  ) where

import Servant
import Servant.Client

import CROSSMAP.API
import CROSSMAP.Login
import CROSSMAP.Session
import CROSSMAP.User


type LoginClientM = LoginRequest -> ClientM LoginResponse


type SessionClientM = GetSessionClientM :<|> DeleteSessionClientM


type UserClientM = GetUserClientM


type UsersClientM = GetUsersClientM :<|> GetUserByIdClientM


type GetSessionClientM = ClientM SessionResponse


type DeleteSessionClientM = ClientM NoContent


type GetUserClientM = ClientM UserResponse


type GetUsersClientM = ClientM [UserId]


type GetUserByIdClientM = UserId -> GetUserClientM


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
userClient :: UserClientM
usersClient :: UsersClientM
sessionClient :<|> userClient :<|> usersClient = client privateAPI


getSessionClient :: GetSessionClientM
deleteSessionClient :: DeleteSessionClientM
getSessionClient :<|> deleteSessionClient = sessionClient


getUserClient :: GetUserClientM
getUserClient = userClient


getUsersClient :: GetUsersClientM
getUserByIdClient :: GetUserByIdClientM
getUsersClient :<|> getUserByIdClient = usersClient
