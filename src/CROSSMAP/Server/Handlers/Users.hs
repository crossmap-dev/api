{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.Users
  ( getUsersHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State
import CROSSMAP.User (UserId(..))


getUsersHandler :: State -> SignatureInfo -> Handler [UserId]
getUsersHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ getUsers
  case result of
    Right users -> return users
    _ -> throwError err500 { errBody = "Database error" }
