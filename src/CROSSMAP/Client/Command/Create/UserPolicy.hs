module CROSSMAP.Client.Command.Create.UserPolicy
  ( CreateUserPolicyCommand(..)
  , createUserPolicyOptions
  , runCreateUserPolicy
  ) where

import Data.Text (Text)
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Policy
import CROSSMAP.User


data CreateUserPolicyCommand = CreateUserPolicyCommand
  { createUserPolicyUser :: Text
  , createUserPolicyPolicy :: Text
  } deriving (Show)


createUserPolicyOptions :: Parser CreateUserPolicyCommand
createUserPolicyOptions = CreateUserPolicyCommand
  <$> argument str
      ( metavar "USER"
     <> help "The user identifier" )
  <*> argument str
      ( metavar "POLICY"
     <> help "The policy identifier" )


runCreateUserPolicy :: CreateUserPolicyCommand -> IO ()
runCreateUserPolicy (CreateUserPolicyCommand user policy) = do
  let maybeUserId = fromText user
  let maybePolicyId = fromText policy
  maybeState <- loadState
  case (maybeUserId, maybePolicyId, maybeState) of
    (Just uid, Just pid, Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ addUserPolicyClient (UserId uid) (PolicyId pid)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "User policy created."
    (Nothing, _, _) ->
      putStrLn "Invalid user identifier."
    (_, Nothing, _) ->
      putStrLn "Invalid policy identifier."
    (_, _, Nothing) ->
      putStrLn "Client not logged in."
