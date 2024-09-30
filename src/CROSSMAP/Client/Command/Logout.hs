module CROSSMAP.Client.Command.Logout
  ( LogoutCommand(..)
  , logoutOptions
  , runLogout
  ) where

import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State


data LogoutCommand = LogoutCommand deriving (Show)


logoutOptions :: Parser LogoutCommand
logoutOptions = pure LogoutCommand


runLogout :: LogoutCommand -> IO ()
runLogout LogoutCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client deleteSessionClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ -> do
          putStrLn "Logout successful."
          clearState
