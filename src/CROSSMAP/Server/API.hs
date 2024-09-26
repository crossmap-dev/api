module CROSSMAP.Server.API
  ( server
  ) where

import Servant

import CROSSMAP.API
import CROSSMAP.Server.Handlers


server :: Server API
server = publicServer :<|> loginServer :<|> secureServer


publicServer :: Server PublicAPI
publicServer = indexHandler


loginServer :: Server SecureUserAPI
loginServer = loginHandler


secureServer :: Server SecureSessionAPI
secureServer = sessionHandler
