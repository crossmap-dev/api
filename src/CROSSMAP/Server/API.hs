module CROSSMAP.Server.API
  ( server
  ) where

import Servant

import CROSSMAP.API
import CROSSMAP.Server.Auth
import CROSSMAP.Server.Handlers
import CROSSMAP.Server.State


server :: State -> Server API
server state = publicServer state :<|> loginServer state :<|> secureServer state


publicServer :: State -> Server PublicAPI
publicServer = indexHandler


loginServer :: State -> Server SecureUserAPI
loginServer = loginHandler


secureServer :: State -> Server SecureSessionAPI
secureServer state sig
  = sessionServer state sig
  :<|> userServer state sig
  :<|> usersServer state sig


sessionServer :: State -> SignatureInfo -> Server SessionAPI
sessionServer state sig = getSessionHandler state sig :<|> deleteSessionHandler state sig


userServer :: State -> SignatureInfo -> Server UserAPI
userServer state sig = getUserHandler state sig


usersServer :: State -> SignatureInfo -> Server UsersAPI
usersServer state sig = getUsersHandler state sig :<|> getUserByIdHandler state sig
