module CROSSMAP.Client.API
  ( loginClient
  , sessionClient
  ) where

import Servant.Client

import CROSSMAP.API
import CROSSMAP.Login
import CROSSMAP.Session


type LoginClientM = LoginRequest -> ClientM LoginResponse


type SessionClientM = ClientM SessionResponse


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
sessionClient = client privateAPI
