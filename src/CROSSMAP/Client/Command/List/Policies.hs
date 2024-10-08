module CROSSMAP.Client.Command.List.Policies
  ( ListPoliciesCommand(..)
  , listPoliciesOptions
  , runListPolicies
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Policy


data ListPoliciesCommand = ListPoliciesCommand deriving (Show)


listPoliciesOptions :: Parser ListPoliciesCommand
listPoliciesOptions = pure ListPoliciesCommand


runListPolicies :: ListPoliciesCommand -> IO ()
runListPolicies ListPoliciesCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getPoliciesClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right policies -> do
          putStrLn "Policies:"
          mapM_ (putStrLn . ("- " <>) . unpack . toText . unPolicyId) policies
