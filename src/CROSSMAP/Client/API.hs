module CROSSMAP.Client.API
  ( loginClient
  ) where

import Servant.Client

import CROSSMAP.API
import CROSSMAP.Login


type LoginClientM = LoginRequest -> ClientM LoginResponse


loginClient :: LoginClientM
loginClient = client loginAPI
