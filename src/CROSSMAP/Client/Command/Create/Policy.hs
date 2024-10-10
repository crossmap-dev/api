module CROSSMAP.Client.Command.Create.Policy
  ( CreatePolicyCommand(..)
  , createPolicyOptions
  , runCreatePolicy
  ) where

import Data.Text (Text)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Policy


data CreatePolicyCommand = CreatePolicyCommand
  { createPolicyName :: Text
  , createPolicyRules :: [CreatePolicyRule]
  } deriving (Show)


createPolicyOptions :: Parser CreatePolicyCommand
createPolicyOptions = CreatePolicyCommand
  <$> argument str
      ( metavar "POLICYNAME"
     <> help "The name of the policy to create" )
  <*> many (CreatePolicyRule
      <$> switch
          ( long "read"
         <> help "Allow read access" )
      <*> switch
          ( long "write"
         <> help "Allow write access" )
      <*> argument str
          ( metavar "RESOURCE"
         <> help "The resource to allow access to" ))


runCreatePolicy :: CreatePolicyCommand -> IO ()
runCreatePolicy (CreatePolicyCommand name rules) = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      let createPolicyReq = CreatePolicyRequest name rules
      result <- runClient client (createPolicyClient createPolicyReq)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "Policy created."
