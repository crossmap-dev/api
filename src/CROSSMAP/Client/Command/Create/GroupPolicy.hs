module CROSSMAP.Client.Command.Create.GroupPolicy
  ( CreateGroupPolicyCommand(..)
  , createGroupPolicyOptions
  , runCreateGroupPolicy
  ) where

import Data.Text (Text)
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Group
import CROSSMAP.Policy


data CreateGroupPolicyCommand = CreateGroupPolicyCommand
  { createGroupPolicyGroup :: Text
  , createGroupPolicyPolicy :: Text
  } deriving (Show)


createGroupPolicyOptions :: Parser CreateGroupPolicyCommand
createGroupPolicyOptions = CreateGroupPolicyCommand
  <$> argument str
      ( metavar "GROUP"
     <> help "The group identifier" )
  <*> argument str
      ( metavar "POLICY"
     <> help "The policy identifier" )


runCreateGroupPolicy :: CreateGroupPolicyCommand -> IO ()
runCreateGroupPolicy (CreateGroupPolicyCommand group policy) = do
  let maybeGroupId = fromText group
  let maybePolicyId = fromText policy
  maybeState <- loadState
  case (maybeGroupId, maybePolicyId, maybeState) of
    (Just gid, Just pid, Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ addPolicyToGroupClient (GroupId gid) (PolicyId pid)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "Group policy created."
    (Nothing, _, _) ->
      putStrLn "Invalid group identifier."
    (_, Nothing, _) ->
      putStrLn "Invalid policy identifier."
    (_, _, Nothing) ->
      putStrLn "Client not logged in."
