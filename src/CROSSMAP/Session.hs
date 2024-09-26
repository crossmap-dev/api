{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Session
  ( SessionResponse(..)
  ) where

import Data.Aeson
import Data.UUID


data SessionResponse = SessionResponse
  { sessionResponseSessionId :: UUID
  } deriving (Show)


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o -> do
    sessionResponseSessionId <- o .: "sessionId"
    return SessionResponse{..}


instance ToJSON SessionResponse where
  toJSON SessionResponse{..} = object [ "sessionId" .= sessionResponseSessionId ]
