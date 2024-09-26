{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.Socket
  ( websocketHandler
  ) where

import Data.Text
import Network.WebSockets

import CROSSMAP.Server.State


websocketHandler :: State -> PendingConnection -> IO ()
websocketHandler _ pending = do
  conn <- acceptRequest pending
  sendTextData conn ("Hello, client!" :: Text)
  sendClose conn ("Bye!" :: Text)
