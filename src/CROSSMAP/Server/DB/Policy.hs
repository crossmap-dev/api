{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Policy
  ( Policy(..)
  , PolicyId(..)
  , PolicyRule(..)
  , getPolicyById
  , getPolicyByName
  , getPolicies
  , insertPolicy
  , updatePolicy
  , matchPolicyRuleStatement
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Function (on)
import Data.List (groupBy)
import Data.Text (Text)
import Data.UUID (UUID, nil)
import Data.Vector (Vector)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


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


getPolicies :: Transaction [Policy]
getPolicies = do
  rows <- statement () getPoliciesStatement
  return $ processPolicies rows


getPoliciesStatement ::
  Statement () [(UUID, Maybe Text, Maybe UUID, Maybe Bool, Maybe Bool, Maybe Text)]
getPoliciesStatement = Statement sql encoder decoder True
  where
    sql = "SELECT p.uuid, pn.name, pr.rule_uuid, pr.read, pr.write, pr.resource \
          \FROM policies p \
          \LEFT JOIN policies_names pn ON p.uuid = pn.policy_uuid \
          \LEFT JOIN policies_rules pr ON p.uuid = pr.policy_uuid \
          \ORDER BY p.uuid, pn.name, pr.rule_uuid"
    encoder = E.noParams
    decoder = D.rowList $ (,,,,,)
      <$> D.column (D.nonNullable D.uuid)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.uuid)
      <*> D.column (D.nullable D.bool)
      <*> D.column (D.nullable D.bool)
      <*> D.column (D.nullable D.text)


processPolicies ::
  [(UUID, Maybe Text, Maybe UUID, Maybe Bool, Maybe Bool, Maybe Text)] -> [Policy]
processPolicies rows =
  map createPolicy . groupBy ((==) `on` getPolicyUuid) $ rows
  where
    getGroupUuid [] = nil
    getGroupUuid ((uuid, _, _, _, _, _) : _) = uuid
    getPolicyUuid (uuid, _, _, _, _, _) = uuid
    createPolicy group =
      let names = [name | (_, Just name, _, _, _, _) <- group]
          rules = [PolicyRule rUuid read' write resource
                  | (_, _, Just rUuid, Just read', Just write, Just resource) <- group]
      in Policy (PolicyId $ getGroupUuid group) names rules


getPolicyById :: PolicyId -> Transaction (Maybe Policy)
getPolicyById pid = do
  rows <- statement pid getPolicyByIdStatement
  return $ processPolicy rows


getPolicyByIdStatement ::
  Statement PolicyId [(UUID, Maybe Text, Maybe UUID, Maybe Bool, Maybe Bool, Maybe Text)]
getPolicyByIdStatement = Statement sql encoder decoder True
  where
    sql = "SELECT p.uuid, pn.name, pr.rule_uuid, pr.read, pr.write, pr.resource \
          \FROM policies p \
          \LEFT JOIN policies_names pn ON p.uuid = pn.policy_uuid \
          \LEFT JOIN policies_rules pr ON p.uuid = pr.policy_uuid \
          \WHERE p.uuid = $1 \
          \ORDER BY p.uuid, pn.name, pr.rule_uuid"
    encoder = unPolicyId >$< E.param (E.nonNullable E.uuid)
    decoder = D.rowList $ (,,,,,)
      <$> D.column (D.nonNullable D.uuid)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.uuid)
      <*> D.column (D.nullable D.bool)
      <*> D.column (D.nullable D.bool)
      <*> D.column (D.nullable D.text)


getPolicyByName :: Text -> Transaction (Maybe Policy)
getPolicyByName name = do
  rows <- statement name getPolicyByNameStatement
  return $ processPolicy rows


getPolicyByNameStatement ::
  Statement Text [(UUID, Maybe Text, Maybe UUID, Maybe Bool, Maybe Bool, Maybe Text)]
getPolicyByNameStatement = Statement sql encoder decoder True
  where
    sql = "SELECT p.uuid, pn1.name, pr.rule_uuid, pr.read, pr.write, pr.resource \
          \FROM policies p \
          \LEFT JOIN policies_names pn1 ON p.uuid = pn1.policy_uuid \
          \LEFT JOIN policies_names pn2 ON p.uuid = pn2.policy_uuid \
          \LEFT JOIN policies_rules pr ON p.uuid = pr.policy_uuid \
          \WHERE pn2.name = $1 \
          \ORDER BY p.uuid, pn1.name, pr.rule_uuid"
    encoder = E.param (E.nonNullable E.text)
    decoder = D.rowList $ (,,,,,)
      <$> D.column (D.nonNullable D.uuid)
      <*> D.column (D.nullable D.text)
      <*> D.column (D.nullable D.uuid)
      <*> D.column (D.nullable D.bool)
      <*> D.column (D.nullable D.bool)
      <*> D.column (D.nullable D.text)


processPolicy ::
  [(UUID, Maybe Text, Maybe UUID, Maybe Bool, Maybe Bool, Maybe Text)] -> Maybe Policy
processPolicy [] = Nothing
processPolicy rows =
  let names = [name | (_, Just name, _, _, _, _) <- rows]
      rules = [PolicyRule rUuid read' write resource
              | (_, _, Just rUuid, Just read', Just write, Just resource) <- rows]
  in Just $ Policy (PolicyId $ getPolicyUuid rows) names rules
  where
    getPolicyUuid [] = nil
    getPolicyUuid ((uuid, _, _, _, _, _) : _) = uuid


insertPolicy :: Policy -> Transaction ()
insertPolicy policy = do
  statement policy insertPolicyStatement
  mapM_ (insertPolicyRule $ policyId policy) (policyRules policy)
  mapM_ (insertPolicyName $ policyId policy) (policyNames policy)


updatePolicy :: Policy -> Transaction ()
updatePolicy policy = do
  statement policy deletePolicyNamesStatement
  statement policy deletePolicyRulesStatement
  mapM_ (insertPolicyRule $ policyId policy) (policyRules policy)
  mapM_ (insertPolicyName $ policyId policy) (policyNames policy)


deletePolicyNamesStatement :: Statement Policy ()
deletePolicyNamesStatement = Statement sql encoder decoder True
  where
    sql = "DELETE FROM policies_names WHERE policy_uuid = $1"
    encoder = unPolicyId . policyId >$< E.param (E.nonNullable E.uuid)
    decoder = D.noResult


deletePolicyRulesStatement :: Statement Policy ()
deletePolicyRulesStatement = Statement sql encoder decoder True
  where
    sql = "DELETE FROM policies_rules WHERE policy_uuid = $1"
    encoder = unPolicyId . policyId >$< E.param (E.nonNullable E.uuid)
    decoder = D.noResult


insertPolicyStatement :: Statement Policy ()
insertPolicyStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO policies (uuid) VALUES ($1)"
    encoder = unPolicyId . policyId >$< E.param (E.nonNullable E.uuid)
    decoder = D.noResult


insertPolicyRule :: PolicyId -> PolicyRule -> Transaction ()
insertPolicyRule pid rule = statement (pid, rule) insertPolicyRuleStatement


insertPolicyRuleStatement :: Statement (PolicyId, PolicyRule) ()
insertPolicyRuleStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO policies_rules (policy_uuid, rule_uuid, read, write, resource) \
          \VALUES ($1, $2, $3, $4, $5)"
    encoder = ((unPolicyId . fst) >$< E.param (E.nonNullable E.uuid))
           <> ((policyRuleId . snd) >$< E.param (E.nonNullable E.uuid))
           <> ((policyRuleAllowRead . snd) >$< E.param (E.nonNullable E.bool))
           <> ((policyRuleAllowWrite . snd) >$< E.param (E.nonNullable E.bool))
           <> ((policyRuleResource . snd) >$< E.param (E.nonNullable E.text))
    decoder = D.noResult


insertPolicyName :: PolicyId -> Text -> Transaction ()
insertPolicyName pid name = statement (pid, name) insertPolicyNameStatement


insertPolicyNameStatement :: Statement (PolicyId, Text) ()
insertPolicyNameStatement = Statement sql encoder decoder True
  where
    sql = "INSERT INTO policies_names (policy_uuid, name) VALUES ($1, $2)"
    encoder = ((unPolicyId . fst) >$< E.param (E.nonNullable E.uuid))
           <> (snd >$< E.param (E.nonNullable E.text))
    decoder = D.noResult


matchPolicyRuleStatement :: Statement (Vector UUID, Text, Bool, Bool) ([UUID])
matchPolicyRuleStatement = Statement sql encoder decoder True
  where
    sql = "SELECT rule_uuid \
          \FROM policies_rules \
          \WHERE policy_uuid = ANY($1) \
          \AND $2 LIKE resource \
          \AND ((read = TRUE AND $3 = TRUE) OR (write = TRUE AND $4 = TRUE))"
    encoder
      = (policyIds >$< E.param (E.nonNullable $ E.foldableArray (E.nonNullable E.uuid)))
      <> (policyResource >$< E.param (E.nonNullable E.text))
      <> (policyRead >$< E.param (E.nonNullable E.bool))
      <> (policyWrite >$< E.param (E.nonNullable E.bool))
    decoder = D.rowList $ D.column (D.nonNullable D.uuid)
    policyIds (pids, _, _, _) = pids
    policyResource (_, r, _, _) = r
    policyRead (_, _, r, _) = r
    policyWrite (_, _, _, w) = w
