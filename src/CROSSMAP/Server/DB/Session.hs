{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.DB.Session
  ( Session(..)
  , insertSession
  , deleteSession
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

import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.User (UserId(..))


data Session = Session
  { sessionUser :: UserId
  , sessionPublicKey :: PublicKey
  , sessionAddress :: IPRange
  } deriving (Eq, Show)


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


deleteSession :: Session -> Transaction ()
deleteSession session = do
  let UserId{..} = sessionUser session
  statement (unUserId, sessionPublicKey session) deleteSessionStatement
  deletePublicKey (sessionPublicKey session)


deleteSessionStatement :: Statement (UUID, PublicKey) ()
deleteSessionStatement = Statement sql encoder decoder False where
  sql = "DELETE FROM sessions WHERE user_uuid = $1 AND public_key = $2"
  encoder
    =  (fst >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . snd >$< E.param (E.nonNullable E.bytea))
  decoder = D.noResult
