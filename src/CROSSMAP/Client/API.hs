{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.Client.API
  ( loginClient
  , getSessionClient
  , deleteSessionClient
  , getUserClient
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


type GetSessionClientM = ClientM SessionResponse


type DeleteSessionClientM = ClientM NoContent


type GetUserClientM = ClientM UserResponse


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
userClient :: UserClientM
sessionClient :<|> userClient = client privateAPI


getSessionClient :: GetSessionClientM
deleteSessionClient :: DeleteSessionClientM
getSessionClient :<|> deleteSessionClient = sessionClient


getUserClient :: GetUserClientM
getUserClient = userClient
