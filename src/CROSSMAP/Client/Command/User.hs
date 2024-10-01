{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.User
  ( UserCommand(..)
  , userOptions
  , runUser
  ) where

import Data.Text (unpack)
import Data.UUID (toText)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.PublicKey
import CROSSMAP.User


data UserCommand = UserCommand deriving (Show)


userOptions :: Parser UserCommand
userOptions = pure UserCommand


runUser :: UserCommand -> IO ()
runUser UserCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getUserClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right u -> do
          putStrLn $ "User ID: " ++ unpack (toText (userResponseUserId u))
          putStrLn "Usernames:"
          mapM_ (putStrLn . unpack . ("- " <>)) (userResponseUsernames u)
          putStrLn "Public keys:"
          mapM_ putPublicKey (userResponsePublicKeys u)
  where
    putPublicKey UserPublicKey{..} = do
      putStrLn $ "- Public key: " ++ unpack (base64PublicKeyToText userPublicKey)
      putStrLn $ "  Created at: " ++ show userPublicKeyCreatedAt
      putStrLn $ "  Expires at: " ++ show userPublicKeyExpiresAt
