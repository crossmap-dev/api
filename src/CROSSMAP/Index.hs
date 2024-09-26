{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Index
  ( IndexResponse(..)
  ) where

import Data.Aeson


data IndexResponse = IndexResponse
  { message :: String
  } deriving (Show)


instance FromJSON IndexResponse where
  parseJSON = withObject "IndexResponse" $ \o -> do
    message <- o .: "message"
    return IndexResponse{..}


instance ToJSON IndexResponse where
  toJSON IndexResponse{..} = object [ "message" .= message ]
