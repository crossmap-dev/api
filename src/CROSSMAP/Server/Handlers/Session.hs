{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.Session
  ( getSessionHandler
  , deleteSessionHandler
  ) where

import Control.Monad.IO.Class
import Data.IP
import Network.Socket
import Servant

import CROSSMAP.PublicKey
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
