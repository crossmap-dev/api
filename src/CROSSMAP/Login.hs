{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Login
  ( LoginRequest(..)
  , LoginResponse(..)
  ) where

import Data.Aeson
import Data.UUID


data LoginRequest = LoginRequest
  { loginRequestUsername :: String
  } deriving (Show)


instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> do
    loginRequestUsername <- o .: "username"
    return LoginRequest{..}


instance ToJSON LoginRequest where
  toJSON LoginRequest{..} = object [ "username" .= loginRequestUsername ]


data LoginResponse = LoginResponse
  { loginResponseSessionId :: UUID
  } deriving (Show)


instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \o -> do
    loginResponseSessionId <- o .: "sessionId"
    return LoginResponse{..}


instance ToJSON LoginResponse where
  toJSON LoginResponse{..} = object [ "sessionId" .= loginResponseSessionId ]
