{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Helpers
  ( authorize
  , ensureSession
  ) where

import Control.Monad.IO.Class
import Data.Text.Encoding
import Data.Time.Clock
import Network.Wai
import Network.HTTP.Types
import Servant

import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Auth
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.State


authorize :: State -> SignatureInfo -> Handler AuthorizationResult
authorize State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  let isWrite' = isWrite $ requestMethod $ signatureInfoRequest
  let resource = decodeUtf8 $ rawPathInfo signatureInfoRequest
  result <- liftIO $ runQuery pool $ authorizationQuery signatureInfoPolicyIds resource isWrite'
  case result of
    Right (Just r) -> return r
    Right Nothing -> throwError err403 { errBody = "Forbidden" }
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }


ensureSession :: PublicKeyInfo -> Handler ()
ensureSession PublicKeyInfo{..} = do
  now <- liftIO getCurrentTime
  case publicKeyInfoType of
    UserKey -> throwError err401 { errBody = "Not logged in" }
    SessionKey -> if now < publicKeyInfoExpires
      then return ()
      else throwError err401 { errBody = "Session expired" }


isWrite :: Method -> Bool
isWrite m | m == methodGet = False
isWrite m | m == methodHead = False
isWrite m | m == methodOptions = False
isWrite _ = True
