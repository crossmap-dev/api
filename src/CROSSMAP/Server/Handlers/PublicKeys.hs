{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.PublicKeys
  ( createPublicKeyHandler
  , getPublicKeysHandler
  , getPublicKeyHandler
  ) where

import Control.Monad.IO.Class
import Data.Time.Clock
import Servant

import CROSSMAP.Base64PublicKey
import CROSSMAP.PublicKey
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State


createPublicKeyHandler ::
  State -> SignatureInfo -> CreatePublicKeyRequest -> Handler PublicKeyInfo
createPublicKeyHandler State{..} SignatureInfo{..} req = do
  ensureSession signatureInfoPublicKeyInfo
  created <- liftIO getCurrentTime
  let pk = createPublicKeyRequestPublicKey req
  let defaultExpires = addUTCTime (60 * 60 * 24 * 30) created
  let expires = maybe defaultExpires id $ createPublicKeyRequestExpires req
  let user = publicKeyInfoUser signatureInfoPublicKeyInfo
  result <- liftIO $ runUpdate pool $ insertUserPublicKey user created expires pk
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return PublicKeyInfo
      { publicKeyInfoType = UserKey
      , publicKeyInfoUser = user
      , publicKeyInfoCreated = created
      , publicKeyInfoExpires = expires
      , publicKeyInfoPublicKey = pk
      }


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
