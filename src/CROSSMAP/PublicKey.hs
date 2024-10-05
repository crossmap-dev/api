{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.PublicKey
  ( PublicKeyType(..)
  , PublicKeyInfo(..)
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Time.Clock (UTCTime)

import CROSSMAP.User (UserId(..))


data PublicKeyType = UserKey | SessionKey deriving (Eq, Show)


data PublicKeyInfo = PublicKeyInfo
  { publicKeyInfoType :: PublicKeyType
  , publicKeyInfoUser :: UserId
  , publicKeyInfoPublicKey :: PublicKey
  , publicKeyInfoCreated :: UTCTime
  , publicKeyInfoExpires :: UTCTime
  } deriving (Eq, Show)
