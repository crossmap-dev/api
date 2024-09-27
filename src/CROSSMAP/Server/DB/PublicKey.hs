{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.PublicKey
  ( PublicKeyType(..)
  , PublicKeyInfo(..)
  , insertPublicKey
  , lookupPublicKey
  , insertUserPublicKey
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


insertPublicKey :: UTCTime -> UTCTime -> PublicKey -> Transaction ()
insertPublicKey created expires publicKey =
  statement ((created, expires), publicKey) insertPublicKeyStatement


insertPublicKeyStatement :: Statement ((UTCTime, UTCTime), PublicKey) ()
insertPublicKeyStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO public_keys \
        \(created_at, expires_at, public_key) \
        \VALUES ($1, $2, $3)"
  encoder
    =  ((fst . fst) >$< E.param (E.nonNullable E.timestamptz))
    <> ((snd . fst) >$< E.param (E.nonNullable E.timestamptz))
    <> (unPublicKey . snd >$< E.param (E.nonNullable E.bytea))
  decoder = D.noResult


insertUserPublicKey :: User -> UTCTime -> UTCTime -> PublicKey -> Transaction ()
insertUserPublicKey user created expires publicKey = do
  insertPublicKey created expires publicKey
  statement ((created, expires), (user, publicKey)) insertUserPublicKeyStatement


insertUserPublicKeyStatement :: Statement ((UTCTime, UTCTime), (User, PublicKey)) ()
insertUserPublicKeyStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users_public_keys \
        \(created_at, expires_at, user_uuid, public_key) \
        \VALUES ($1, $2, $3, $4)"
  encoder
    =  ((fst . fst) >$< E.param (E.nonNullable E.timestamptz))
    <> ((snd . fst) >$< E.param (E.nonNullable E.timestamptz))
    <> ((userId . fst . snd) >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . snd . snd >$< E.param (E.nonNullable E.bytea))
  decoder = D.noResult


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
