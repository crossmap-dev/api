module CROSSMAP.Server.Handlers
  ( indexHandler
  , loginHandler
  , sessionHandler
  ) where

import Data.UUID
import Servant

import CROSSMAP.Index
import CROSSMAP.Login
import CROSSMAP.Session
import CROSSMAP.Server.Auth
import CROSSMAP.Server.State


indexHandler :: State -> Handler IndexResponse
indexHandler _ = return $ IndexResponse "CROSSMAP"


loginHandler :: State -> SignatureInfo -> LoginRequest -> Handler LoginResponse
loginHandler _ _ _ = return $ LoginResponse nil


sessionHandler :: State -> SignatureInfo -> Handler SessionResponse
sessionHandler _ _ = return $ SessionResponse nil
