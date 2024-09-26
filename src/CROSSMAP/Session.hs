{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Session
  ( SessionResponse(..)
  ) where

import Data.Aeson
import Data.UUID


data SessionResponse = SessionResponse
  { sessionResponseId :: UUID
  } deriving (Show)


instance FromJSON SessionResponse where
  parseJSON = withObject "SessionResponse" $ \o -> do
    sessionResponseId <- o .: "sessionId"
    return SessionResponse{..}


instance ToJSON SessionResponse where
  toJSON SessionResponse{..} = object [ "sessionId" .= sessionResponseId ]
