{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Session
  ( SessionResponse(..)
  ) where

import Data.Aeson
import Data.Time
import Data.UUID

import CROSSMAP.Base64PublicKey


data SessionResponse = SessionResponse
  { sessionResponseSessionUser :: UUID
  , sessionResponseSessionPublicKey :: Base64PublicKey
  , sessionResponseSessionCreatedAt :: UTCTime
  , sessionResponseSessionExpiresAt :: UTCTime
  } deriving (Show)


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o -> do
    sessionResponseSessionUser <- o .: "sessionUser"
    sessionResponseSessionPublicKey <- o .: "sessionPublicKey"
    sessionResponseSessionCreatedAt <- o .: "sessionCreatedAt"
    sessionResponseSessionExpiresAt <- o .: "sessionExpiresAt"
    return SessionResponse{..}


instance ToJSON SessionResponse where
  toJSON SessionResponse{..} = object
    [ "sessionUser" .= sessionResponseSessionUser
    , "sessionPublicKey" .= sessionResponseSessionPublicKey
    , "sessionCreatedAt" .= sessionResponseSessionCreatedAt
    , "sessionExpiresAt" .= sessionResponseSessionExpiresAt
    ]
