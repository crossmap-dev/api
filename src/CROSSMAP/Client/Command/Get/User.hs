{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Get.User
  ( GetUserCommand(..)
  , getUserOptions
  , runGetUser
  ) where

import Data.Text (Text, unpack)
import Data.UUID (toText)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.PublicKey
import CROSSMAP.User


data GetUserCommand = GetUserCommand
  { getUserCommandUsername :: Text
  } deriving (Show)


getUserOptions :: Parser GetUserCommand
getUserOptions = GetUserCommand
  <$> argument str ( metavar "USERNAME" )


runGetUser :: GetUserCommand -> IO ()
runGetUser (GetUserCommand username) = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client $ getUserByUsernameClient username
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right user -> do
          putStrLn $ "User ID: " ++ unpack (toText (userResponseUserId user))
          putStrLn "Usernames:"
          mapM_ (putStrLn . unpack . ("- " <>)) (userResponseUsernames user)
          putStrLn "Public keys:"
          mapM_ putPublicKey (userResponsePublicKeys user)
  where
    putPublicKey UserPublicKey{..} = do
      putStrLn $ "- Public key: " ++ unpack (base64PublicKeyToText userPublicKey)
      putStrLn $ "  Created at: " ++ show userPublicKeyCreatedAt
      putStrLn $ "  Expires at: " ++ show userPublicKeyExpiresAt
