module CROSSMAP.Client.Command.Session
  ( SessionCommand(..)
  , sessionOptions
  , runSession
  ) where

import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State


data SessionCommand = SessionCommand deriving (Show)


sessionOptions :: Parser SessionCommand
sessionOptions = pure SessionCommand


runSession :: SessionCommand -> IO ()
runSession SessionCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client sessionClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right s ->
          print s
