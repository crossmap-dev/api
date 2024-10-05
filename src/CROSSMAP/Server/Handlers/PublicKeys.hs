{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.PublicKeys
  ( getPublicKeysHandler
  , getPublicKeyHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.Base64PublicKey
import CROSSMAP.PublicKey
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State


getPublicKeysHandler :: State -> SignatureInfo -> Handler [Base64PublicKey]
getPublicKeysHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ getPublicKeys
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right publicKeys -> return $ fmap Base64PublicKey publicKeys


getPublicKeyHandler :: State -> SignatureInfo -> Base64PublicKey -> Handler PublicKeyInfo
getPublicKeyHandler State{..} SignatureInfo{..} (Base64PublicKey publicKey) = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ lookupPublicKey publicKey
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right Nothing -> throwError err404 { errBody = "Public key not found" }
    Right (Just publicKeyInfo) -> return publicKeyInfo
