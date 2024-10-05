{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Login
  ( LoginRequest(..)
  , LoginResponse(..)
  ) where

import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID

import CROSSMAP.Base64PublicKey


data LoginRequest = LoginRequest
  { loginRequestUsername :: Text
  , loginRequestSessionPublicKey :: Base64PublicKey
  } deriving (Show)


instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> do
    loginRequestUsername <- o .: "username"
    loginRequestSessionPublicKey <- o .: "sessionPublicKey"
    return LoginRequest{..}


instance ToJSON LoginRequest where
  toJSON LoginRequest{..} = object
    [ "username" .= loginRequestUsername
    , "sessionPublicKey" .= loginRequestSessionPublicKey
    ]


data LoginResponse = LoginResponse
  { loginResponseSessionUser :: UUID
  , loginResponseSessionPublicKey :: Base64PublicKey
  , loginResponseSessionCreatedAt :: UTCTime
  , loginResponseSessionExpiresAt :: UTCTime
  } deriving (Show)


instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \o -> do
    loginResponseSessionUser <- o .: "sessionUser"
    loginResponseSessionPublicKey <- o .: "sessionPublicKey"
    loginResponseSessionCreatedAt <- o .: "sessionCreatedAt"
    loginResponseSessionExpiresAt <- o .: "sessionExpiresAt"
    return LoginResponse{..}


instance ToJSON LoginResponse where
  toJSON LoginResponse{..} = object
    [ "sessionUser" .= loginResponseSessionUser
    , "sessionPublicKey" .= loginResponseSessionPublicKey
    , "sessionCreatedAt" .= loginResponseSessionCreatedAt
    , "sessionExpiresAt" .= loginResponseSessionExpiresAt
    ]
