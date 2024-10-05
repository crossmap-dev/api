{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.Handlers
  ( indexHandler
  , module CROSSMAP.Server.Handlers.Login
  , module CROSSMAP.Server.Handlers.PublicKeys
  , module CROSSMAP.Server.Handlers.Session
  , module CROSSMAP.Server.Handlers.Sessions
  , module CROSSMAP.Server.Handlers.User
  , module CROSSMAP.Server.Handlers.Users
  ) where

import Servant

import CROSSMAP.Index (IndexResponse(IndexResponse))
import CROSSMAP.Server.Handlers.Login
import CROSSMAP.Server.Handlers.PublicKeys
import CROSSMAP.Server.Handlers.Session
import CROSSMAP.Server.Handlers.Sessions
import CROSSMAP.Server.Handlers.User
import CROSSMAP.Server.Handlers.Users
import CROSSMAP.Server.State
import CROSSMAP.Version


indexHandler :: State -> Handler IndexResponse
indexHandler _ = return $ IndexResponse "CROSSMAP" version
