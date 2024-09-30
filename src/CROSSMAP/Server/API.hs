module CROSSMAP.Server.API
  ( server
  ) where

import Servant

import CROSSMAP.API
import CROSSMAP.Server.Handlers
import CROSSMAP.Server.State


server :: State -> Server API
server state = publicServer state :<|> loginServer state :<|> secureServer state


publicServer :: State -> Server PublicAPI
publicServer = indexHandler


loginServer :: State -> Server SecureUserAPI
loginServer = loginHandler


secureServer :: State -> Server SecureSessionAPI
secureServer state sig = getSessionHandler state sig :<|> deleteSessionHandler state sig
