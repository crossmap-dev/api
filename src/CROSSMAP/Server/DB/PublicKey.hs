{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.PublicKey
  ( PublicKeyType(..)
  , PublicKeyInfo(..)
  , insertPublicKey
  , deletePublicKey
  , lookupPublicKey
  , listUserPublicKeys
  , insertUserPublicKey
  , deleteUserPublicKey
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Base64PublicKey (Base64PublicKey(..))
import CROSSMAP.PublicKey
import CROSSMAP.User (UserId(..), UserPublicKey(..))


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


deletePublicKey :: PublicKey -> Transaction ()
deletePublicKey publicKey = statement publicKey deletePublicKeyStatement


deletePublicKeyStatement :: Statement PublicKey ()
deletePublicKeyStatement = Statement sql encoder decoder False where
  sql = "DELETE FROM public_keys WHERE public_key = $1"
  encoder = unPublicKey >$< E.param (E.nonNullable E.bytea)
  decoder = D.noResult


insertUserPublicKey :: UserId -> UTCTime -> UTCTime -> PublicKey -> Transaction ()
insertUserPublicKey user created expires publicKey = do
  insertPublicKey created expires publicKey
  statement (user, publicKey) insertUserPublicKeyStatement


insertUserPublicKeyStatement :: Statement (UserId, PublicKey) ()
insertUserPublicKeyStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users_public_keys \
        \(user_uuid, public_key) \
        \VALUES ($1, $2)"
  encoder
    =  ((unUserId . fst) >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . snd >$< E.param (E.nonNullable E.bytea))
  decoder = D.noResult


deleteUserPublicKey :: UserId -> PublicKey -> Transaction ()
deleteUserPublicKey user publicKey = do
  statement (user, publicKey) deleteUserPublicKeyStatement
  deletePublicKey publicKey


deleteUserPublicKeyStatement :: Statement (UserId, PublicKey) ()
deleteUserPublicKeyStatement = Statement sql encoder decoder False where
  sql = "DELETE FROM users_public_keys \
        \WHERE user_uuid = $1 AND public_key = $2"
  encoder
    =  (unUserId . fst >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . snd >$< E.param (E.nonNullable E.bytea))
  decoder = D.noResult


lookupPublicKey :: PublicKey -> Transaction (Maybe PublicKeyInfo)
lookupPublicKey = flip statement lookupPublicKeyStatement


lookupPublicKeyStatement :: Statement PublicKey (Maybe PublicKeyInfo)
lookupPublicKeyStatement = Statement sql encoder decoder True where
  sql = "SELECT 'user' as key_type, \
        \  users_public_keys.user_uuid, \
        \  users_public_keys.public_key, \
        \  public_keys.created_at, \
        \  public_keys.expires_at \
        \FROM \
        \  users_public_keys \
        \JOIN \
        \  public_keys \
        \ON \
        \  users_public_keys.public_key = public_keys.public_key \
        \WHERE \
        \  users_public_keys.public_key = $1 \
        \UNION ALL \
        \SELECT 'session' as key_type, \
        \  sessions.user_uuid, \
        \  sessions.public_key, \
        \  public_keys.created_at, \
        \  public_keys.expires_at \
        \FROM \
        \  sessions \
        \JOIN \
        \  public_keys \
        \ON \
        \  sessions.public_key = public_keys.public_key \
        \WHERE \
        \  sessions.public_key = $1"
  encoder = unPublicKey >$< E.param (E.nonNullable E.bytea)
  decoder = D.rowMaybe publicKeyInfoDecoder


listUserPublicKeys :: UserId -> Transaction [UserPublicKey]
listUserPublicKeys user = statement user listUserPublicKeysStatement


listUserPublicKeysStatement :: Statement UserId [UserPublicKey]
listUserPublicKeysStatement = Statement sql encoder decoder True where
  sql = "SELECT \
        \  users_public_keys.public_key, \
        \  public_keys.created_at, \
        \  public_keys.expires_at \
        \FROM \
        \  users_public_keys \
        \JOIN \
        \  public_keys \
        \ON \
        \  users_public_keys.public_key = public_keys.public_key \
        \WHERE \
        \  users_public_keys.user_uuid = $1"
  encoder = unUserId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList $ UserPublicKey <$>
    D.column (D.nonNullable ((Base64PublicKey . PublicKey) <$> D.bytea)) <*>
    D.column (D.nonNullable D.timestamptz) <*>
    D.column (D.nonNullable D.timestamptz)


publicKeyInfoDecoder :: D.Row PublicKeyInfo
publicKeyInfoDecoder = PublicKeyInfo
  <$> D.column (D.nonNullable publicKeyTypeDecoder)
  <*> D.column (D.nonNullable (UserId <$> D.uuid))
  <*> D.column (D.nonNullable (PublicKey <$> D.bytea))
  <*> D.column (D.nonNullable D.timestamptz)
  <*> D.column (D.nonNullable D.timestamptz)


publicKeyTypeDecoder :: D.Value PublicKeyType
publicKeyTypeDecoder = D.enum $ \case
  "user" -> Just UserKey
  "session" -> Just SessionKey
  _ -> Nothing
