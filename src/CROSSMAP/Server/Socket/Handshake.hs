{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Socket.Handshake
  ( handshake
  ) where

import Data.Aeson
import Data.UUID.V4
import Network.WebSockets

import CROSSMAP.Server.Client
import CROSSMAP.Server.State


data Handshake = Handshake
  { handshakeClientId :: ClientId
  } deriving (Eq, Show)


instance FromJSON Handshake where
  parseJSON = withObject "Handshake" $ \o -> do
    handshakeClientId <- ClientId <$> o .: "clientId"
    return Handshake{..}


instance ToJSON Handshake where
  toJSON Handshake{..} = object
    [ "clientId" .= unClientId handshakeClientId
    ]


handshake :: State -> PendingConnection -> IO Connection
handshake State{..} pending = do
  conn <- acceptRequest pending
  handshakeClientId <- ClientId <$> nextRandom
  sendBinaryData conn $ encode Handshake{..}
  return conn
