{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Auth
  ( AuthorizationResult(..)
  , authorizationQuery
  , PublicKeyInfoPolicyRow(..)
  , publicKeyInfoPolicyQuery
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Policy
import CROSSMAP.PublicKey
import CROSSMAP.User (UserId(..))


data AuthorizationResult = AuthorizationResult
  { authorizationResultPolicyId :: PolicyId
  , authorizationResultRuleUuid :: UUID
  } deriving (Eq, Show)


data PublicKeyInfoPolicyRow = PublicKeyInfoPolicyRow
  { publicKeyInfoPolicyRowPublicKeyInfo :: PublicKeyInfo
  , publicKeyInfoPolicyRowPolicyIds :: [PolicyId]
  } deriving (Eq, Show)


authorizationQuery ::
  [PolicyId] -> Text -> Bool -> Transaction (Maybe AuthorizationResult)
authorizationQuery policyIds resource isWrite =
  statement (policyIds, resource, isWrite) authorizationQueryStatement


authorizationQueryStatement ::
  Statement ([PolicyId], Text, Bool) (Maybe AuthorizationResult)
authorizationQueryStatement = Statement sql encoder decoder True where
  sql = "SELECT \
        \  pr.policy_uuid, \
        \  pr.rule_uuid \
        \FROM unnest($1::uuid[]) AS p(policy_uuid) \
        \JOIN LATERAL find_matching_policies($2, $3, ARRAY[p.policy_uuid]) mpr ON true \
        \JOIN policies_rules pr \
        \  ON mpr.policy_uuid = pr.policy_uuid \
        \  AND mpr.rule_uuid = pr.rule_uuid \
        \GROUP BY pr.policy_uuid, pr.rule_uuid \
        \LIMIT 1"
  encoder :: E.Params ([PolicyId], Text, Bool)
  encoder
    = ((\(pids, _, _) -> pids) >$< policyIdsEncoder)
    <> ((\(_, r, _) -> r) >$< E.param (E.nonNullable E.text))
    <> ((\(_, _, w) -> w) >$< E.param (E.nonNullable E.bool))
  decoder = D.rowMaybe $ AuthorizationResult
    <$> (PolicyId <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.uuid)
  policyIdsEncoder
    = f >$< E.param (E.nonNullable (E.foldableArray (E.nonNullable E.uuid)))
    where f = map unPolicyId


publicKeyInfoPolicyQuery :: PublicKey -> Transaction (Maybe PublicKeyInfoPolicyRow)
publicKeyInfoPolicyQuery publicKey =
  statement publicKey publicKeyInfoPolicyQueryStatement


publicKeyInfoPolicyQueryStatement :: Statement PublicKey (Maybe PublicKeyInfoPolicyRow)
publicKeyInfoPolicyQueryStatement = Statement sql encoder decoder True where
  sql = "SELECT \
        \  upkr.key_type, \
        \  upkr.public_key, \
        \  upkr.created_at, \
        \  upkr.expires_at, \
        \  upkr.user_uuid, \
        \  ARRAY_AGG(DISTINCT upr.policy_uuid) AS policy_uuids \
        \FROM user_public_key_resolution upkr \
        \LEFT JOIN user_policy_resolution upr \
        \  ON upkr.user_uuid = upr.user_uuid \
        \WHERE upkr.public_key = $1 \
        \GROUP BY \
        \  upkr.key_type, \
        \  upkr.public_key, \
        \  upkr.created_at, \
        \  upkr.expires_at, \
        \  upkr.user_uuid"
  encoder = unPublicKey >$< E.param (E.nonNullable E.bytea)
  decoder :: D.Result (Maybe PublicKeyInfoPolicyRow)
  decoder = D.rowMaybe $ PublicKeyInfoPolicyRow
    <$> (publicKeyInfo
      <$> D.column (D.nonNullable D.text)
      <*> (PublicKey <$> D.column (D.nonNullable D.bytea))
      <*> D.column (D.nonNullable D.timestamptz)
      <*> D.column (D.nonNullable D.timestamptz)
      <*> (UserId <$> D.column (D.nonNullable D.uuid))
    )
    <*> (mapPolicyIds <$> D.column (D.nonNullable (D.listArray (D.nullable D.uuid))))
  mapPolicyIds (Just pid:rest) = PolicyId pid : mapPolicyIds rest
  mapPolicyIds (Nothing:rest) = mapPolicyIds rest
  mapPolicyIds [] = []

publicKeyInfo :: Text -> PublicKey -> UTCTime -> UTCTime -> UserId -> PublicKeyInfo
publicKeyInfo keyType pk created expires userId =
  case keyType of
    "user" -> PublicKeyInfo UserKey userId pk created expires
    "session" -> PublicKeyInfo SessionKey userId pk created expires
    _ -> error $ "Invalid key type: " ++ show keyType
