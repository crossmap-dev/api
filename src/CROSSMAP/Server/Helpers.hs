{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Helpers
  ( ensureSession
  ) where

import Control.Monad.IO.Class
import Data.Time.Clock
import Servant

import CROSSMAP.Server.DB.PublicKey


ensureSession :: PublicKeyInfo -> Handler ()
ensureSession PublicKeyInfo{..} = do
  now <- liftIO getCurrentTime
  case publicKeyInfoType of
    UserKey -> throwError err401 { errBody = "Not logged in" }
    SessionKey -> if now < publicKeyInfoExpires
      then return ()
      else throwError err401 { errBody = "Session expired" }
