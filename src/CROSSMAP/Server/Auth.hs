{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.Auth
  ( UserSignature(..)
  , SessionSignature(..)
  , authContext
  ) where

import Control.Monad.IO.Class (liftIO)
import Network.Wai (Request)
import Servant
import Servant.Server.Experimental.Auth


data UserSignature = UserSignature
  deriving (Eq, Show)


data SessionSignature = SessionSignature
  deriving (Eq, Show)


type instance AuthServerData (AuthProtect "user-signature") = UserSignature


type instance AuthServerData (AuthProtect "session-signature") = SessionSignature


authContext :: Context
  (AuthHandler Request UserSignature ': AuthHandler Request SessionSignature ': '[])
authContext = userAuthHandler :. sessionAuthHandler :. EmptyContext


userAuthHandler :: AuthHandler Request UserSignature
userAuthHandler = mkAuthHandler $ \_ -> do
  liftIO $ putStrLn "TODO: Implement user auth handler"
  return UserSignature


sessionAuthHandler :: AuthHandler Request SessionSignature
sessionAuthHandler = mkAuthHandler $ \_ -> do
  liftIO $ putStrLn "TODO: Implement session auth handler"
  return SessionSignature
