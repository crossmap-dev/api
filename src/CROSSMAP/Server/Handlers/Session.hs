{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.Session
  ( getSessionHandler
  , getSessionByPublicKeyHandler
  , deleteSessionHandler
  , deleteSessionByPublicKeyHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.Base64PublicKey
import CROSSMAP.Session
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.Session
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State
import CROSSMAP.User (UserId(..))


getSessionHandler :: State -> SignatureInfo -> Handler SessionResponse
getSessionHandler _ SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  return $ SessionResponse
    { sessionResponseSessionUser =
      unUserId $ publicKeyInfoUser signatureInfoPublicKeyInfo
    , sessionResponseSessionPublicKey =
      Base64PublicKey $ publicKeyInfoPublicKey signatureInfoPublicKeyInfo
    , sessionResponseSessionCreatedAt =
      publicKeyInfoCreated signatureInfoPublicKeyInfo
    , sessionResponseSessionExpiresAt =
      publicKeyInfoExpires signatureInfoPublicKeyInfo
    }


getSessionByPublicKeyHandler ::
  State -> SignatureInfo -> Base64PublicKey -> Handler SessionResponse
getSessionByPublicKeyHandler State{..} SignatureInfo{..} (Base64PublicKey pk) = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ getSession pk
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right Nothing -> throwError err404 { errBody = "Session not found" }
    Right (Just session) -> return session


deleteSessionHandler :: State -> SignatureInfo -> Handler NoContent
deleteSessionHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  let PublicKeyInfo{..} = signatureInfoPublicKeyInfo
  result <- liftIO $ runUpdate pool $ deleteSession publicKeyInfoUser publicKeyInfoPublicKey
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return NoContent


deleteSessionByPublicKeyHandler ::
  State -> SignatureInfo -> Base64PublicKey -> Handler NoContent
deleteSessionByPublicKeyHandler State{..} SignatureInfo{..} (Base64PublicKey pk) = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runUpdate pool $ deleteSessionByPublicKey pk
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return NoContent
