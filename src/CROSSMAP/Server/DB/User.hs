{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.User
  ( User(..)
  , Username(..)
  , UserPublicKey(..)
  , getUser
  , getUserByUsername
  , getUsernames
  , getUserPublicKeys
  , insertUser
  , userHasName
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


newtype User = User { userId :: UUID } deriving (Eq, Show)


getUser :: UUID -> Transaction (Maybe User)
getUser uuid = statement uuid getUserStatement


getUserStatement :: Statement UUID (Maybe User)
getUserStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM users WHERE uuid = $1"
  encoder = E.param (E.nonNullable E.uuid)
  decoder = D.rowMaybe (User <$> D.column (D.nonNullable D.uuid))


getUserByUsername :: Text -> Transaction (Maybe User)
getUserByUsername = flip statement getUserByUsernameStatement


getUserByUsernameStatement :: Statement Text (Maybe User)
getUserByUsernameStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid FROM user_names WHERE name = $1"
  encoder = E.param (E.nonNullable E.text)
  decoder = D.rowMaybe (User <$> D.column (D.nonNullable D.uuid))


insertUser :: User -> Transaction ()
insertUser = flip statement insertUserStatement


insertUserStatement :: Statement User ()
insertUserStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users (uuid) VALUES ($1)"
  encoder = userId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult


data Username = Username { usernameUser :: User, username :: Text } deriving (Eq, Show)


getUsernames :: User -> Transaction [Username]
getUsernames = flip statement getUsernamesStatement


getUsernamesStatement :: Statement User [Username]
getUsernamesStatement = Statement sql encoder decoder True where
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


getUserPublicKeys :: User -> Transaction [UserPublicKey]
getUserPublicKeys = flip statement getUserPublicKeysStatement


getUserPublicKeysStatement :: Statement User [UserPublicKey]
getUserPublicKeysStatement = Statement sql encoder decoder True where
  sql = "SELECT public_key, user_uuid, created_at, expires_at \
        \FROM user_public_keys WHERE user_uuid = $1"
  encoder = userId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList userPublicKeyDecoder
  userPublicKeyDecoder = UserPublicKey
    <$> (PublicKey <$> D.column (D.nonNullable D.bytea))
    <*> (User <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)


userHasName :: User -> Text -> Transaction Bool
userHasName user name = do
  names <- getUsernames user
  return $ any ((== name) . username) names
