{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers
  ( indexHandler
  , loginHandler
  , getSessionHandler
  , deleteSessionHandler
  ) where

import Control.Monad.IO.Class
import Data.IP
import Data.Time.Clock
import Network.Socket
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
      let expires = addUTCTime 3600 now
      result' <- liftIO $ runUpdate pool $ insertSession now expires $ Session
        { sessionUser = publicKeyInfoUser
        , sessionPublicKey = sessionPublicKey
        , sessionAddress = case signatureInfoSocketAddr of
            SockAddrInet _ addr ->
              IPv4Range $ makeAddrRange (fromHostAddress addr) 32
            SockAddrInet6 _ _ addr _ ->
              IPv6Range $ makeAddrRange (fromHostAddress6 addr) 128
            _ -> error "Unsupported socket address"
        }
      case result' of
        Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
        Right () -> return $ LoginResponse
          { loginResponseSessionUser = userId publicKeyInfoUser
          , loginResponseSessionPublicKey = Base64PublicKey sessionPublicKey
          , loginResponseSessionCreatedAt = now
          , loginResponseSessionExpiresAt = expires
          }


getSessionHandler :: State -> SignatureInfo -> Handler SessionResponse
getSessionHandler _ SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  return $ SessionResponse
    { sessionResponseSessionUser =
      userId $ publicKeyInfoUser signatureInfoPublicKeyInfo
    , sessionResponseSessionPublicKey =
      Base64PublicKey $ publicKeyInfoPublicKey signatureInfoPublicKeyInfo
    , sessionResponseSessionCreatedAt =
      publicKeyInfoCreated signatureInfoPublicKeyInfo
    , sessionResponseSessionExpiresAt =
      publicKeyInfoExpires signatureInfoPublicKeyInfo
    }


deleteSessionHandler :: State -> SignatureInfo -> Handler NoContent
deleteSessionHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  let PublicKeyInfo{..} = signatureInfoPublicKeyInfo
  result <- liftIO $ runUpdate pool $ deleteSession $ Session
    { sessionUser = publicKeyInfoUser
    , sessionPublicKey = publicKeyInfoPublicKey
    , sessionAddress = case signatureInfoSocketAddr of
        SockAddrInet _ addr ->
          IPv4Range $ makeAddrRange (fromHostAddress addr) 32
        SockAddrInet6 _ _ addr _ ->
          IPv6Range $ makeAddrRange (fromHostAddress6 addr) 128
        _ -> error "Unsupported socket address"
    }
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return NoContent


ensureSession :: PublicKeyInfo -> Handler ()
ensureSession PublicKeyInfo{..} = do
  now <- liftIO getCurrentTime
  case publicKeyInfoType of
    UserKey -> throwError err401 { errBody = "Not logged in" }
    SessionKey -> if now < publicKeyInfoExpires
      then return ()
      else throwError err401 { errBody = "Session expired" }
