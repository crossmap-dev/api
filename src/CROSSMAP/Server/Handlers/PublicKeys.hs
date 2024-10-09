{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.PublicKeys
  ( createPublicKeyHandler
  , deletePublicKeyHandler
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
createPublicKeyHandler state@State{..} signatureInfo@SignatureInfo{..} req = do
  _ <- authorize state signatureInfo
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


deletePublicKeyHandler :: State -> SignatureInfo -> Base64PublicKey -> Handler NoContent
deletePublicKeyHandler state@State{..} signatureInfo (Base64PublicKey publicKey) = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runUpdate pool $ deletePublicKey publicKey
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return NoContent


getPublicKeysHandler :: State -> SignatureInfo -> Handler [Base64PublicKey]
getPublicKeysHandler state@State{..} signatureInfo = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runQuery pool $ getPublicKeys
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right publicKeys -> return $ fmap Base64PublicKey publicKeys


getPublicKeyHandler :: State -> SignatureInfo -> Base64PublicKey -> Handler PublicKeyInfo
getPublicKeyHandler state@State{..} signatureInfo (Base64PublicKey publicKey) = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runQuery pool $ lookupPublicKey publicKey
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right Nothing -> throwError err404 { errBody = "Public key not found" }
    Right (Just publicKeyInfo) -> return publicKeyInfo
