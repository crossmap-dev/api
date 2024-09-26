{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Session
  ( SessionResponse(..)
  ) where

import Data.Aeson


data SessionResponse = SessionResponse
  { session :: String
  } deriving (Show)


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o -> do
    session <- o .: "session"
    return SessionResponse{..}


instance ToJSON SessionResponse where
  toJSON SessionResponse{..} = object [ "session" .= session ]
