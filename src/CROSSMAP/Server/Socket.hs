{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Socket
  ( websocketHandler
  ) where

import Data.Text
import Network.WebSockets

import CROSSMAP.Event
import CROSSMAP.Server.Socket.Handshake
import CROSSMAP.Server.State


websocketHandler :: State -> PendingConnection -> IO ()
websocketHandler state pending = do
  conn <- handshake state pending
  handleMessages state conn
  sendClose conn ("Bye!" :: Text)


handleMessages :: State -> Connection -> IO ()
handleMessages state conn = do
  msg <- receiveData conn
  case parseEvent msg of
    Nothing -> sendClose conn ("Invalid message" :: Text)
    Just evt -> do
      print evt
      handleMessages state conn
