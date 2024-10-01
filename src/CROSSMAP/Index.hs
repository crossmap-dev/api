{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Index
  ( IndexResponse(..)
  ) where

import Data.Aeson
import Data.Text


data IndexResponse = IndexResponse
  { message :: Text
  , version :: Text
  } deriving (Show)


instance FromJSON IndexResponse where
  parseJSON = withObject "IndexResponse" $ \o -> do
    message <- o .: "message"
    version <- o .: "version"
    return IndexResponse{..}


instance ToJSON IndexResponse where
  toJSON IndexResponse{..} = object [ "message" .= message, "version" .= version ]
