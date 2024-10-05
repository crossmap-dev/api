{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.State
  ( State(..)
  , clientToState
  , loadStateFilePath
  , loadSessionFromState
  , loadState
  , saveState
  , clearState
  ) where

import Data.Aeson
import Data.Text
import System.Environment.XDG.BaseDir
import System.Directory

import CROSSMAP.Base64PublicKey
import CROSSMAP.Client
import CROSSMAP.SecretKey


data State = State
  { stateURL :: Text
  , stateUsername :: Text
  , stateUserPublicKey :: Text
  , stateUserSecretKey :: Text
  , stateSessionPublicKey :: Text
  , stateSessionSecretKey :: Text
  } deriving (Show)


instance FromJSON State where
  parseJSON = withObject "State" $ \o -> State
    <$> o .: "url"
    <*> o .: "username"
    <*> o .: "userPublicKey"
    <*> o .: "userSecretKey"
    <*> o .: "sessionPublicKey"
    <*> o .: "sessionSecretKey"


instance ToJSON State where
  toJSON State{..} = object
    [ "url" .= stateURL
    , "username" .= stateUsername
    , "userPublicKey" .= stateUserPublicKey
    , "userSecretKey" .= stateUserSecretKey
    , "sessionPublicKey" .= stateSessionPublicKey
    , "sessionSecretKey" .= stateSessionSecretKey
    ]


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


loadStateFilePath :: IO FilePath
loadStateFilePath = do
  dataDir <- getUserDataDir "crossmap"
  let stateFilePath = dataDir ++ "/state.json"
  createDirectoryIfMissing True dataDir
  pure stateFilePath


loadState :: IO (Maybe State)
loadState = do
  stateFilePath <- loadStateFilePath
  exists <- doesFileExist stateFilePath
  if exists
    then do
      state <- decodeFileStrict stateFilePath
      case state of
        Just s -> do
          pure $ Just s
        Nothing -> do
          putStrLn "State file is corrupted."
          pure Nothing
    else do
      pure Nothing


saveState :: State -> IO ()
saveState s = do
  stateFilePath <- loadStateFilePath
  encodeFile stateFilePath s
  pure ()


clearState :: IO ()
clearState = do
  stateFilePath <- loadStateFilePath
  removeFile stateFilePath
  pure ()
