module CROSSMAP.Server.App
  ( app
  ) where

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Servant hiding (respond)

import CROSSMAP.API
import CROSSMAP.Server.API
import CROSSMAP.Server.Auth
import CROSSMAP.Server.Socket
import CROSSMAP.Server.State


app :: State -> Application
app state req respond =
  case websocketsApp defaultConnectionOptions socketHandler req of
    Just response -> respond response
    Nothing -> logStdout application req respond
  where
    authContext' = authContext state
    application = serveWithContext api authContext' $ server state
    socketHandler = websocketHandler state
