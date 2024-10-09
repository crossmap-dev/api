{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Auth
  ( PublicKeyInfoPolicyRow(..)
  , publicKeyInfoPolicyQuery
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Policy
import CROSSMAP.PublicKey
import CROSSMAP.User (UserId(..))


data PublicKeyInfoPolicyRow = PublicKeyInfoPolicyRow
  { publicKeyInfoPolicyRowPublicKeyInfo :: PublicKeyInfo
  , publicKeyInfoPolicyRowPolicyIds :: [PolicyId]
  } deriving (Eq, Show)


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
