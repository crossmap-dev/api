module CROSSMAP.Client.Command.List.UserPolicies
  ( ListUserPoliciesCommand(..)
  , listUserPoliciesOptions
  , runListUserPolicies
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.User
import CROSSMAP.Policy


data ListUserPoliciesCommand = ListUserPoliciesCommand
  { listUserPoliciesUser :: Text }
  deriving (Show)


listUserPoliciesOptions :: Parser ListUserPoliciesCommand
listUserPoliciesOptions = ListUserPoliciesCommand
  <$> argument str
      ( metavar "USER"
     <> help "The user identifier" )


runListUserPolicies :: ListUserPoliciesCommand -> IO ()
runListUserPolicies (ListUserPoliciesCommand grp) = do
  let maybeUserId = fromText grp
  maybeState <- loadState
  case (maybeUserId, maybeState) of
    (Just gid, Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ getUserPoliciesClient (UserId gid)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right policies -> do
          putStrLn "Policies:"
          mapM_ (putStrLn . ("- " <>) . unpack . toText . unPolicyId) policies
    (Nothing, _) ->
      putStrLn "Invalid user identifier."
    (_, Nothing) ->
      putStrLn "Client not logged in."
