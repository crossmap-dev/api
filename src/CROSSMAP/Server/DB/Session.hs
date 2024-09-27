{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Session
  ( Session(..)
  , insertSession
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Functor.Contravariant ((>$<))
import Data.Time.Clock (UTCTime)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Server.DB.User (User(..))


data Session = Session
  { sessionUser :: User
  , sessionPublicKey :: PublicKey
  , sessionCreated :: UTCTime
  , sessionExpires :: UTCTime
  } deriving (Eq, Show)


insertSession :: Session -> Transaction ()
insertSession = flip statement insertSessionStatement


insertSessionStatement :: Statement Session ()
insertSessionStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO sessions \
        \(user_uuid, public_key, created_at, expires_at) \
        \VALUES ($1, $2, $3, $4)"
  encoder
    =  ((userId . sessionUser) >$< E.param (E.nonNullable E.uuid))
    <> (unPublicKey . sessionPublicKey >$< E.param (E.nonNullable E.bytea))
    <> (sessionCreated >$< E.param (E.nonNullable E.timestamptz))
    <> (sessionExpires >$< E.param (E.nonNullable E.timestamptz))
  decoder = D.noResult
