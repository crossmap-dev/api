{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Auth
  ( authQuery
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


data AuthQueryRow = AuthQueryRow
  { authQueryRowPublicKey :: PublicKeyInfo
  , authQueryRowPolicyRule :: Maybe (PolicyId, PolicyRule)
  } deriving (Eq, Show)


authQuery :: PublicKey -> Text -> Bool -> Transaction (Maybe PublicKeyInfo, [PolicyRule])
authQuery publicKey resource isWrite =
  processRows <$> statement (publicKey, resource, isWrite) authQueryStatement


authQueryStatement :: Statement (PublicKey, Text, Bool) [AuthQueryRow]
authQueryStatement = Statement sql encoder decoder True where
  sql = "SELECT \
        \  upkr.key_type, \
        \  upkr.public_key, \
        \  upkr.created_at, \
        \  upkr.expires_at, \
        \  upkr.user_uuid, \
        \  pr.policy_uuid, \
        \  pr.rule_uuid, \
        \  pr.read, \
        \  pr.write, \
        \  pr.resource \
        \FROM user_public_key_resolution upkr \
        \JOIN user_policy_resolution upr ON upkr.user_uuid = upr.user_uuid \
        \JOIN LATERAL find_matching_policies($2, $3, ARRAY_AGG(upr.policy_uuid)) mp ON true \
        \JOIN policies_rules pr \
        \  ON mp.policy_uuid = pr.policy_uuid \
        \  AND mp.rule_uuid = pr.rule_uuid \
        \WHERE upkr.public_key = $1"
  pkParam (PublicKey pk, _, _) = pk
  resourceParam (_, resource, _) = resource
  isWriteParam (_, _, isWrite) = isWrite
  encoder
    = (pkParam >$< E.param (E.nonNullable E.bytea))
    <> (resourceParam >$< E.param (E.nonNullable E.text))
    <> (isWriteParam >$< E.param (E.nonNullable E.bool))
  decoder = D.rowList $ AuthQueryRow
    <$> (publicKeyInfo
      <$> D.column (D.nonNullable D.text)
      <*> (PublicKey <$> D.column (D.nonNullable D.bytea))
      <*> D.column (D.nonNullable D.timestamptz)
      <*> D.column (D.nonNullable D.timestamptz)
      <*> D.column (D.nonNullable D.uuid)
      )
    <*> (maybePolicyRuleRow
      <$> (D.column (D.nullable D.uuid))
      <*> (D.column (D.nullable D.uuid))
      <*> (D.column (D.nullable D.bool))
      <*> (D.column (D.nullable D.bool))
      <*> (D.column (D.nullable D.text))
      )


publicKeyInfo :: Text -> PublicKey -> UTCTime -> UTCTime -> UUID -> PublicKeyInfo
publicKeyInfo "user" pk created expires userUuid =
  PublicKeyInfo UserKey (UserId userUuid) pk created expires
publicKeyInfo "session" pk created expires userUuid =
  PublicKeyInfo SessionKey (UserId userUuid) pk created expires
publicKeyInfo _ _ _ _ _ = error "Invalid key type"


maybePolicyRuleRow ::
  Maybe UUID -> Maybe UUID -> Maybe Bool -> Maybe Bool -> Maybe Text ->
    Maybe (PolicyId, PolicyRule)
maybePolicyRuleRow mpUuid mrUuid mRead mWrite mResource = do
  pUuid <- mpUuid
  rUuid <- mrUuid
  read' <- mRead
  write <- mWrite
  resource <- mResource
  return (PolicyId pUuid, PolicyRule rUuid read' write resource)


processRows :: [AuthQueryRow] -> (Maybe PublicKeyInfo, [PolicyRule])
processRows [] = (Nothing, [])
processRows rows@(AuthQueryRow pk _ : _) =
  (Just pk, [pr | AuthQueryRow _ (Just (_, pr)) <- rows])
