{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Policy
  ( CreatePolicyRequest(..)
  , CreatePolicyRule(..)
  , createPolicy
  , Policy(..)
  , PolicyId(..)
  , PolicyRule(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import Servant


data CreatePolicyRequest = CreatePolicyRequest
  { createPolicyName :: Text
  , createPolicyRules :: [CreatePolicyRule]
  } deriving (Eq, Show)


instance FromJSON CreatePolicyRequest where
  parseJSON = withObject "CreatePolicyRequest" $ \o -> do
    createPolicyName <- o .: "name"
    createPolicyRules <- o .: "rules"
    return CreatePolicyRequest{..}


instance ToJSON CreatePolicyRequest where
  toJSON CreatePolicyRequest{..} = object
    [ "name" .= createPolicyName
    , "rules" .= createPolicyRules
    ]


data CreatePolicyRule = CreatePolicyRule
  { createPolicyRuleAllowRead :: Bool
  , createPolicyRuleAllowWrite :: Bool
  , createPolicyRuleResource :: Text
  } deriving (Eq, Show)


instance FromJSON CreatePolicyRule where
  parseJSON = withObject "CreatePolicyRule" $ \o -> do
    createPolicyRuleAllowRead <- o .: "read"
    createPolicyRuleAllowWrite <- o .: "write"
    createPolicyRuleResource <- o .: "resource"
    return CreatePolicyRule{..}


instance ToJSON CreatePolicyRule where
  toJSON CreatePolicyRule{..} = object
    [ "read" .= createPolicyRuleAllowRead
    , "write" .= createPolicyRuleAllowWrite
    , "resource" .= createPolicyRuleResource
    ]


createPolicy :: CreatePolicyRequest -> IO Policy
createPolicy CreatePolicyRequest{..} = do
  policyId <- PolicyId <$> nextRandom
  let policyNames = [createPolicyName]
  policyRules <- traverse createPolicyRule createPolicyRules
  return Policy{..}


createPolicyRule :: CreatePolicyRule -> IO PolicyRule
createPolicyRule CreatePolicyRule{..} = do
  policyRuleId <- nextRandom
  let policyRuleAllowRead = createPolicyRuleAllowRead
  let policyRuleAllowWrite = createPolicyRuleAllowWrite
  let policyRuleResource = createPolicyRuleResource
  return PolicyRule{..}


newtype PolicyId = PolicyId { unPolicyId :: UUID } deriving (Eq, Show)


instance FromHttpApiData PolicyId where
  parseUrlPiece s = case fromText s of
    Just uuid -> Right (PolicyId uuid)
    Nothing -> Left "Invalid UUID"


instance FromJSON PolicyId where
  parseJSON = withText "PolicyId" $ \s -> case fromText s of
    Just uuid -> return (PolicyId uuid)
    Nothing -> fail "Invalid UUID"


instance ToHttpApiData PolicyId where
  toUrlPiece (PolicyId uuid) = toText uuid


instance ToJSON PolicyId where
  toJSON (PolicyId uuid) = String (toText uuid)


data Policy = Policy
  { policyId :: PolicyId
  , policyNames :: [Text]
  , policyRules :: [PolicyRule]
  } deriving (Eq, Show)


instance FromJSON Policy where
  parseJSON = withObject "Policy" $ \o -> do
    policyId <- o .: "id"
    policyNames <- o .: "names"
    policyRules <- o .: "rules"
    return Policy{..}


instance ToJSON Policy where
  toJSON Policy{..} = object
    [ "id" .= policyId
    , "names" .= policyNames
    , "rules" .= policyRules
    ]


data PolicyRule = PolicyRule
  { policyRuleId :: UUID
  , policyRuleAllowRead :: Bool
  , policyRuleAllowWrite :: Bool
  , policyRuleResource :: Text
  } deriving (Eq, Show)


instance FromJSON PolicyRule where
  parseJSON = withObject "PolicyRule" $ \o -> do
    policyRuleId <- o .: "id"
    policyRuleAllowRead <- o .: "read"
    policyRuleAllowWrite <- o .: "write"
    policyRuleResource <- o .: "resource"
    return PolicyRule{..}


instance ToJSON PolicyRule where
  toJSON PolicyRule{..} = object
    [ "id" .= policyRuleId
    , "read" .= policyRuleAllowRead
    , "write" .= policyRuleAllowWrite
    , "resource" .= policyRuleResource
    ]
