module CROSSMAP.Client.Command.Create.Group
  ( CreateGroupCommand(..)
  , createGroupOptions
  , runCreateGroup
  ) where

import Data.Text (Text)
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Group
import CROSSMAP.User


data CreateGroupCommand = CreateGroupCommand
  { createGroupNames :: [Text]
  , createGroupMembers :: [Text]
  } deriving (Show)


createGroupOptions :: Parser CreateGroupCommand
createGroupOptions = CreateGroupCommand
  <$> many (argument str
      ( metavar "GROUPNAME"
     <> help "The group name to create" ))
  <*> many (strOption
      ( long "member"
      <> short 'm'
      <> metavar "USERNAME"
      <> help "The username to add to the group" ))


runCreateGroup :: CreateGroupCommand -> IO ()
runCreateGroup (CreateGroupCommand names users) = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client $ createGroupClient $
        CreateGroupRequest names (map parseUser users)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "Group created."
  where
    parseUser s = case fromText s of
      Nothing -> UserIdentifierUsername s
      Just u -> UserIdentifierUserId $ UserId u
