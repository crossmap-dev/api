module CROSSMAP.Client.Command.Get.Policy
  ( GetPolicyCommand(..)
  , getPolicyOptions
  , runGetPolicy
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Policy


data GetPolicyCommand = GetPolicyCommand
  { getPolicyCommandPolicy :: Text
  } deriving (Show)


getPolicyOptions :: Parser GetPolicyCommand
getPolicyOptions = GetPolicyCommand
  <$> argument str ( metavar "POLICY" )


runGetPolicy :: GetPolicyCommand -> IO ()
runGetPolicy (GetPolicyCommand policyText) = do
  let maybePolicyId = fromText policyText
  maybeState <- loadState
  case (maybePolicyId, maybeState) of
    (Nothing, _) ->
      putStrLn "Invalid policy."
    (_, Nothing) ->
      putStrLn "Client not logged in."
    (Just policyId', Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ getPolicy (getPolicyClientById $ PolicyId policyId')
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right policy -> do
          putStrLn $ "Policy: " ++ unpack (toText $ unPolicyId $ policyId policy)
          putStrLn "Names:"
          mapM_ (putStrLn . ("- " <>) . unpack) (policyNames policy)
          putStrLn "Rules:"
          mapM_ (putStrLn . ("- " <>) . showPolicyRule) (policyRules policy)
  where
    showPolicyRule (PolicyRule ruleId allowRead allowWrite resource) =
      "Rule: " ++ unpack (toText ruleId) ++
        ", allow read: " ++ show allowRead ++
        ", allow write: " ++ show allowWrite ++
        ", resource: " ++ unpack resource
