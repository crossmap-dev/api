{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.State
  ( State(..)
  , clientToState
  , loadSessionFromState
  , loadState
  , saveState
  ) where

import Data.Text

import CROSSMAP.Client
import CROSSMAP.PublicKey
import CROSSMAP.SecretKey


data State = State
  { stateURL :: Text
  , stateUsername :: Text
  , stateUserPublicKey :: Text
  , stateUserSecretKey :: Text
  , stateSessionPublicKey :: Text
  , stateSessionSecretKey :: Text
  } deriving (Show)


clientToState :: Client -> State
clientToState client = State
  { stateURL = clientURL client
  , stateUsername = clientUsername client
  , stateUserPublicKey = publicKeyToText $ clientUserPublicKey client
  , stateUserSecretKey = secretKeyToText $ clientUserSecretKey client
  , stateSessionPublicKey = publicKeyToText $ clientSessionPublicKey client
  , stateSessionSecretKey = secretKeyToText $ clientSessionSecretKey client
  }


loadSessionFromState :: State -> IO Client
loadSessionFromState State{..} =
  loadSession stateURL stateUsername
    stateUserPublicKey stateUserSecretKey
    stateSessionPublicKey stateSessionSecretKey


loadState :: IO (Maybe State)
loadState = do
  putStrLn "Loading state..."
  putStrLn "TODO: Implement loading state..."
  pure Nothing


saveState :: State -> IO ()
saveState _ = do
  putStrLn "Saving state..."
  putStrLn "TODO: Implement saving state..."
  pure ()
