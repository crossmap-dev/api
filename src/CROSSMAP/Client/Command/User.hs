module CROSSMAP.Client.Command.User
  ( UserCommand(..)
  , userOptions
  , runUser
  ) where

import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State


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
          print u
