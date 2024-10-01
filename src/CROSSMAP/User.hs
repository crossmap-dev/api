{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.User
  ( UserResponse(..)
  , UserPublicKey(..)
  ) where

import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID

import CROSSMAP.PublicKey


data UserResponse = UserResponse
  { userResponseUserId :: UUID
  , userResponseUsernames :: [Text]
  , userResponsePublicKeys :: [UserPublicKey]
  } deriving (Eq, Show)


instance FromJSON UserResponse where
  parseJSON = withObject "UserResponse" $ \o -> do
    userResponseUserId <- o .: "id"
    userResponseUsernames <- o .: "usernames"
    userResponsePublicKeys <- o .: "publicKeys"
    return UserResponse{..}


instance ToJSON UserResponse where
  toJSON UserResponse{..} = object
    [ "id" .= userResponseUserId
    , "usernames" .= userResponseUsernames
    , "publicKeys" .= userResponsePublicKeys
    ]


data UserPublicKey = UserPublicKey
  { userPublicKey :: Base64PublicKey
  , userPublicKeyCreatedAt :: UTCTime
  , userPublicKeyExpiresAt :: UTCTime
  } deriving (Eq, Show)


instance FromJSON UserPublicKey where
  parseJSON = withObject "UserPublicKey" $ \o -> do
    userPublicKey <- o .: "publicKey"
    userPublicKeyCreatedAt <- o .: "createdAt"
    userPublicKeyExpiresAt <- o .: "expiresAt"
    return UserPublicKey{..}


instance ToJSON UserPublicKey where
  toJSON UserPublicKey{..} = object
    [ "publicKey" .= userPublicKey
    , "createdAt" .= userPublicKeyCreatedAt
    , "expiresAt" .= userPublicKeyExpiresAt
    ]
