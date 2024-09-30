{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.Client.API
  ( loginClient
  , getSessionClient
  , deleteSessionClient
  ) where

import Servant
import Servant.Client

import CROSSMAP.API
import CROSSMAP.Login
import CROSSMAP.Session


type LoginClientM = LoginRequest -> ClientM LoginResponse


type SessionClientM = GetSessionClientM :<|> DeleteSessionClientM


type GetSessionClientM = ClientM SessionResponse


type DeleteSessionClientM = ClientM NoContent


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
sessionClient = client privateAPI


getSessionClient :: GetSessionClientM
deleteSessionClient :: DeleteSessionClientM
getSessionClient :<|> deleteSessionClient = sessionClient
