{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Login
  ( LoginRequest(..)
  , LoginResponse(..)
  ) where

import Data.Aeson
import Data.Text

import CROSSMAP.PublicKey


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
  { loginResponseSessionPublicKey :: Base64PublicKey
  } deriving (Show)


instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \o -> do
    loginResponseSessionPublicKey <- o .: "sessionPublicKey"
    return LoginResponse{..}


instance ToJSON LoginResponse where
  toJSON LoginResponse{..} = object [ "sessionPublicKey" .= loginResponseSessionPublicKey ]
