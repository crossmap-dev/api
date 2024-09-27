{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Session
  ( Session(..)
  , insertSession
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.IP
import Data.Time.Clock (UTCTime)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.User (User(..))


data Session = Session
  { sessionUser :: User
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
    =  ((userId . sessionUser) >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . sessionPublicKey >$< E.param (E.nonNullable E.bytea))
    <> (sessionAddress >$< E.param (E.nonNullable E.inet))
  decoder = D.noResult
