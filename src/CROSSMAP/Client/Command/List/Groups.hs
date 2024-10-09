module CROSSMAP.Client.Command.List.Groups
  ( ListGroupsCommand(..)
  , listGroupsOptions
  , runListGroups
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Group


data ListGroupsCommand = ListGroupsCommand deriving (Show)


listGroupsOptions :: Parser ListGroupsCommand
listGroupsOptions = pure ListGroupsCommand


runListGroups :: ListGroupsCommand -> IO ()
runListGroups ListGroupsCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getGroupsClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right groups -> do
          putStrLn "Groups:"
          mapM_ (putStrLn . ("- " <>) . unpack . toText . unGroupId) groups
