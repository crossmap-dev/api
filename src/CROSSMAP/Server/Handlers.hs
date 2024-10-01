{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.Handlers
  ( indexHandler
  , loginHandler
  , getSessionHandler
  , deleteSessionHandler
  , getUserHandler
  , getUsersHandler
  , getUserByIdHandler
  , getUserByUsernameHandler
  ) where

import Servant

import CROSSMAP.Index (IndexResponse(IndexResponse))
import CROSSMAP.Server.Handlers.Login
import CROSSMAP.Server.Handlers.Session
import CROSSMAP.Server.Handlers.User
import CROSSMAP.Server.Handlers.Users
import CROSSMAP.Server.State
import CROSSMAP.Version


indexHandler :: State -> Handler IndexResponse
indexHandler _ = return $ IndexResponse "CROSSMAP" version
