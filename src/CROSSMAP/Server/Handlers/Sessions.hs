{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.Sessions
  ( getSessionsHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.PublicKey
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Session
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State


getSessionsHandler :: State -> SignatureInfo -> Handler [Base64PublicKey]
getSessionsHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ getSessions
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right sessions -> return $ fmap Base64PublicKey sessions
