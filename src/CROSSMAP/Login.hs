{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Login
  ( LoginRequest(..)
  , LoginResponse(..)
  ) where

import Data.Aeson


data LoginRequest = LoginRequest
  { username :: String
  } deriving (Show)


instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> do
    username <- o .: "username"
    return LoginRequest{..}


instance ToJSON LoginRequest where
  toJSON LoginRequest{..} = object [ "username" .= username ]


data LoginResponse = LoginResponse
  { session :: String
  } deriving (Show)


instance FromJSON LoginResponse where
  parseJSON = withObject "LoginResponse" $ \o -> do
    session <- o .: "session"
    return LoginResponse{..}


instance ToJSON LoginResponse where
  toJSON LoginResponse{..} = object [ "session" .= session ]
