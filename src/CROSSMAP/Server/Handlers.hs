{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers
  ( indexHandler
  , loginHandler
  , sessionHandler
  ) where

import Control.Monad.IO.Class
import Data.Time.Clock
import Data.UUID
import Servant

import CROSSMAP.Index
import CROSSMAP.Login
import CROSSMAP.PublicKey
import CROSSMAP.Session
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.Session
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.State


indexHandler :: State -> Handler IndexResponse
indexHandler _ = return $ IndexResponse "CROSSMAP"


loginHandler :: State -> SignatureInfo -> LoginRequest -> Handler LoginResponse
loginHandler State{..} SignatureInfo{..} loginReq = do
  let PublicKeyInfo{..} = signatureInfoPublicKeyInfo
  let loginUsername = loginRequestUsername loginReq
  let Base64PublicKey sessionPublicKey = loginRequestSessionPublicKey loginReq
  result <- liftIO $ runQuery pool $ userHasName publicKeyInfoUser loginUsername
  case result of
    Left _ -> throwError err500
    Right False -> throwError err401
    Right True -> do
      now <- liftIO getCurrentTime
      result' <- liftIO $ runUpdate pool $ insertSession $ Session
        { sessionUser = publicKeyInfoUser
        , sessionPublicKey = sessionPublicKey
        , sessionCreated = now
        , sessionExpires = addUTCTime 3600 now
        }
      case result' of
        Left _ -> throwError err500
        Right () -> return $ LoginResponse $ Base64PublicKey sessionPublicKey


sessionHandler :: State -> SignatureInfo -> Handler SessionResponse
sessionHandler _ _ = return $ SessionResponse nil
