{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.PublicKey
  ( PublicKeyType(..)
  , PublicKeyInfo(..)
  , lookupPublicKey
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Server.DB.User (User(..))


data PublicKeyType = UserKey | SessionKey deriving (Eq, Show)


data PublicKeyInfo = PublicKeyInfo
  { publicKeyInfoType :: PublicKeyType
  , publicKeyInfoUser :: User
  , publicKeyInfoPublicKey :: PublicKey
  , publicKeyInfoCreated :: UTCTime
  , publicKeyInfoExpires :: UTCTime
  } deriving (Eq, Show)


lookupPublicKey :: PublicKey -> Transaction (Maybe PublicKeyInfo)
lookupPublicKey = flip statement lookupPublicKeyStatement


lookupPublicKeyStatement :: Statement PublicKey (Maybe PublicKeyInfo)
lookupPublicKeyStatement = Statement sql encoder decoder True where
  sql = "SELECT 'user' as key_type, \
        \  user_uuid, public_key, created_at, expires_at \
        \FROM \
        \  users_public_keys \
        \WHERE \
        \  public_key = $1 \
        \UNION ALL \
        \SELECT 'session' as key_type, \
        \  user_uuid, public_key, created_at, expires_at \
        \FROM \
        \  sessions \
        \WHERE \
        \  public_key = $1"
  encoder = unPublicKey >$< E.param (E.nonNullable E.bytea)
  decoder = D.rowMaybe publicKeyInfoDecoder


publicKeyInfoDecoder :: D.Row PublicKeyInfo
publicKeyInfoDecoder = PublicKeyInfo
  <$> D.column (D.nonNullable publicKeyTypeDecoder)
  <*> D.column (D.nonNullable (User <$> D.uuid))
  <*> D.column (D.nonNullable (PublicKey <$> D.bytea))
  <*> D.column (D.nonNullable D.timestamptz)
  <*> D.column (D.nonNullable D.timestamptz)


publicKeyTypeDecoder :: D.Value PublicKeyType
publicKeyTypeDecoder = D.enum $ \case
  "user" -> Just UserKey
  "session" -> Just SessionKey
  _ -> Nothing
