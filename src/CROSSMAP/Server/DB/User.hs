{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.User
  ( User(..)
  , Username(..)
  , getUser
  , getUserNames
  ) where

import Crypto.Sign.Ed25519 (PublicKey)
import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


newtype User = User { userId :: UUID } deriving (Eq, Show)


getUser :: UUID -> Transaction (Maybe User)
getUser uuid = statement uuid getUserStatement


getUserStatement :: Statement UUID (Maybe User)
getUserStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM users WHERE uuid = $1"
  encoder = E.param (E.nonNullable E.uuid)
  decoder = D.rowMaybe (User <$> D.column (D.nonNullable D.uuid))


data Username = Username { usernameUser :: User, username :: Text } deriving (Eq, Show)


getUserNames :: User -> Transaction [Username]
getUserNames = flip statement getUserNamesStatement


getUserNamesStatement :: Statement User [Username]
getUserNamesStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid, name FROM user_names WHERE user_uuid = $1"
  encoder = userId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList userNameDecoder
  userNameDecoder = Username
    <$> (User <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.text)


data UserPublicKey = UserPublicKey
  { userPublicKey :: PublicKey
  , userPublicKeyUser :: User
  , userPublicKeyCreated :: UTCTime
  , userPublicKeyExpires :: UTCTime
  } deriving (Eq, Show)
