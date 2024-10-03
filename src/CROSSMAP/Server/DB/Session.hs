{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.DB.Session
  ( Session(..)
  , getSession
  , getSessions
  , insertSession
  , deleteSession
  , deleteSessionByPublicKey
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.IP
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.PublicKey (Base64PublicKey(..))
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Session
import CROSSMAP.User (UserId(..))


data Session = Session
  { sessionUser :: UserId
  , sessionPublicKey :: PublicKey
  , sessionAddress :: IPRange
  } deriving (Eq, Show)


getSessions :: Transaction [PublicKey]
getSessions = statement () getSessionsStatement


getSessionsStatement :: Statement () [PublicKey]
getSessionsStatement = Statement sql encoder decoder True where
  sql = "SELECT public_key FROM sessions"
  encoder = E.noParams
  decoder = D.rowList (PublicKey <$> D.column (D.nonNullable D.bytea))


getSession :: PublicKey -> Transaction (Maybe SessionResponse)
getSession publicKey = statement publicKey getSessionStatement


getSessionStatement :: Statement PublicKey (Maybe SessionResponse)
getSessionStatement = Statement sql encoder decoder True where
  sql = "SELECT \
        \  sessions.user_uuid, \
        \  sessions.public_key, \
        \  public_keys.created_at, \
        \  public_keys.expires_at \
        \FROM \
        \  sessions \
        \WHERE \
        \  public_key = $1"
  encoder = unPublicKey >$< E.param (E.nonNullable E.bytea)
  decoder = D.rowMaybe $ SessionResponse
    <$> (D.column (D.nonNullable D.uuid))
    <*> (Base64PublicKey . PublicKey <$> D.column (D.nonNullable D.bytea))
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)


insertSession :: UTCTime -> UTCTime -> Session -> Transaction ()
insertSession created expires session = do
  insertPublicKey created expires (sessionPublicKey session)
  statement session insertSessionStatement


insertSessionStatement :: Statement Session ()
insertSessionStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO sessions \
        \(user_uuid, public_key, address) \
        \VALUES ($1, $2, $3)"
  encoder
    =  ((unUserId . sessionUser) >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . sessionPublicKey >$< E.param (E.nonNullable E.bytea))
    <> (sessionAddress >$< E.param (E.nonNullable E.inet))
  decoder = D.noResult


deleteSession :: UserId -> PublicKey -> Transaction ()
deleteSession uid pk = do
  statement (unUserId uid, pk) deleteSessionStatement
  deletePublicKey (pk)


deleteSessionStatement :: Statement (UUID, PublicKey) ()
deleteSessionStatement = Statement sql encoder decoder False where
  sql = "DELETE FROM sessions WHERE user_uuid = $1 AND public_key = $2"
  encoder
    =  (fst >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . snd >$< E.param (E.nonNullable E.bytea))
  decoder = D.noResult


deleteSessionByPublicKey :: PublicKey -> Transaction ()
deleteSessionByPublicKey pk = do
  statement pk deleteSessionByPublicKeyStatement
  deletePublicKey pk


deleteSessionByPublicKeyStatement :: Statement PublicKey ()
deleteSessionByPublicKeyStatement = Statement sql encoder decoder False where
  sql = "DELETE FROM sessions WHERE public_key = $1"
  encoder = unPublicKey >$< E.param (E.nonNullable E.bytea)
  decoder = D.noResult
