module CROSSMAP.Client.Command.List.Users
  ( ListUsersCommand(..)
  , listUsersOptions
  , runListUsers
  ) where

import Data.Text
import Data.UUID (toText)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.User


data ListUsersCommand = ListUsersCommand deriving (Show)


listUsersOptions :: Parser ListUsersCommand
listUsersOptions = pure ListUsersCommand


runListUsers :: ListUsersCommand -> IO ()
runListUsers ListUsersCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getUsersClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right users -> do
          putStrLn "Users:"
          mapM_ (putStrLn . ("- " <>) . unpack . toText . unUserId) users
