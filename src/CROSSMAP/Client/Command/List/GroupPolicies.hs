module CROSSMAP.Client.Command.List.GroupPolicies
  ( ListGroupPoliciesCommand(..)
  , listGroupPoliciesOptions
  , runListGroupPolicies
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Group
import CROSSMAP.Policy


data ListGroupPoliciesCommand = ListGroupPoliciesCommand
  { listGroupPoliciesGroup :: Text }
  deriving (Show)


listGroupPoliciesOptions :: Parser ListGroupPoliciesCommand
listGroupPoliciesOptions = ListGroupPoliciesCommand
  <$> argument str
      ( metavar "GROUP"
     <> help "The group identifier" )


runListGroupPolicies :: ListGroupPoliciesCommand -> IO ()
runListGroupPolicies (ListGroupPoliciesCommand grp) = do
  let maybeGroupId = fromText grp
  maybeState <- loadState
  case (maybeGroupId, maybeState) of
    (Just gid, Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ getGroupPoliciesClient (GroupId gid)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right policies -> do
          putStrLn "Policies:"
          mapM_ (putStrLn . ("- " <>) . unpack . toText . unPolicyId) policies
    (Nothing, _) ->
      putStrLn "Invalid group identifier."
    (_, Nothing) ->
      putStrLn "Client not logged in."
