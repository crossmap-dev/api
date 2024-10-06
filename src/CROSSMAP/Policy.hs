module CROSSMAP.Policy
  ( Policy(..)
  , PolicyId(..)
  , PolicyRule(..)
  ) where

import Data.Text (Text)
import Data.UUID (UUID)


newtype PolicyId = PolicyId { unPolicyId :: UUID } deriving (Eq, Show)


data Policy = Policy
  { policyId :: PolicyId
  , policyNames :: [Text]
  , policyRules :: [PolicyRule]
  } deriving (Eq, Show)


data PolicyRule = PolicyRule
  { policyRuleId :: UUID
  , policyRuleAllowRead :: Bool
  , policyRuleAllowWrite :: Bool
  , policyRuleResource :: Text
  } deriving (Eq, Show)
