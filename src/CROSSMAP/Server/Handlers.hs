module CROSSMAP.Server.Handlers
  ( indexHandler
  , loginHandler
  , sessionHandler
  ) where

import Servant

import CROSSMAP.Index
import CROSSMAP.Login
import CROSSMAP.Session
import CROSSMAP.Server.Auth


indexHandler :: Handler IndexResponse
indexHandler = return $ IndexResponse "CROSSMAP"


loginHandler :: UserSignature -> LoginRequest -> Handler LoginResponse
loginHandler _ loginReq = return $ LoginResponse $ username loginReq


sessionHandler :: SessionSignature -> Handler SessionResponse
sessionHandler _ = return $ SessionResponse "placeholder"
