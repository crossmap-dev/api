{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.PublicKey
  ( PublicKeyType(..)
  , PublicKeyInfo(..)
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Aeson
import Data.Time.Clock (UTCTime)

import CROSSMAP.Base64PublicKey (Base64PublicKey(..))
import CROSSMAP.User (UserId(..))


data PublicKeyType = UserKey | SessionKey deriving (Eq, Show)


instance ToJSON PublicKeyType where
  toJSON UserKey = "user"
  toJSON SessionKey = "session"


instance FromJSON PublicKeyType where
  parseJSON = withText "PublicKeyType" $ \case
    "user" -> return UserKey
    "session" -> return SessionKey
    _ -> fail "Invalid PublicKeyType"


data PublicKeyInfo = PublicKeyInfo
  { publicKeyInfoType :: PublicKeyType
  , publicKeyInfoUser :: UserId
  , publicKeyInfoPublicKey :: PublicKey
  , publicKeyInfoCreated :: UTCTime
  , publicKeyInfoExpires :: UTCTime
  } deriving (Eq, Show)


instance ToJSON PublicKeyInfo where
  toJSON (PublicKeyInfo typ user key created expires) =
    object
      [ "type" .= typ
      , "user" .= user
      , "key" .= Base64PublicKey key
      , "created" .= created
      , "expires" .= expires
      ]


instance FromJSON PublicKeyInfo where
  parseJSON = withObject "PublicKeyInfo" $ \o -> do
    typ <- o .: "type"
    user <- o .: "user"
    base64key <- o .: "key"
    created <- o .: "created"
    expires <- o .: "expires"
    let Base64PublicKey key = base64key
    return $ PublicKeyInfo typ user key created expires
