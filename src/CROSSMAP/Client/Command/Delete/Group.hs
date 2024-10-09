module CROSSMAP.Client.Command.Delete.Group
  ( DeleteGroupCommand(..)
  , deleteGroupOptions
  , runDeleteGroup
  ) where

import Data.Text (Text)
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Group


data DeleteGroupCommand = DeleteGroupCommand
  { deleteGroupCommandGroup :: Text
  } deriving (Show)


deleteGroupOptions :: Parser DeleteGroupCommand
deleteGroupOptions = DeleteGroupCommand
  <$> argument str ( metavar "GROUP" )


runDeleteGroup :: DeleteGroupCommand -> IO ()
runDeleteGroup (DeleteGroupCommand groupText) = do
  let maybeGroupId = fromText groupText
  maybeState <- loadState
  case (maybeGroupId, maybeState) of
    (_, Nothing) ->
      putStrLn "Client not logged in."
    (Nothing, _) ->
      putStrLn "Invalid group."
    (Just groupId', Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ deleteGroup (getGroupClientById $ GroupId groupId')
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "Group deleted."
