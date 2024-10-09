{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.User
  ( CreateUserRequest(..)
  , UserResponse(..)
  , UserPublicKey(..)
  , UserId(..)
  , UserIdentifier(..)
  ) where

import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID
import Servant

import CROSSMAP.Base64PublicKey


data CreateUserRequest = CreateUserRequest
  { createUserRequestUsernames :: [Text]
  , createUserRequestPublicKeys :: [Base64PublicKey]
  } deriving (Eq, Show)


instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \o -> do
    createUserRequestUsernames <- o .: "usernames"
    createUserRequestPublicKeys <- o .: "publicKeys"
    return CreateUserRequest{..}


instance ToJSON CreateUserRequest where
  toJSON CreateUserRequest{..} = object
    [ "usernames" .= createUserRequestUsernames
    , "publicKeys" .= createUserRequestPublicKeys
    ]


newtype UserId = UserId { unUserId :: UUID } deriving (Eq, Show)


instance FromHttpApiData UserId where
  parseUrlPiece t = case fromText t of
    Just uuid -> Right $ UserId uuid
    Nothing -> Left "Invalid UUID"


instance FromJSON UserId where
  parseJSON (String s) = case fromText s of
    Just uuid -> return $ UserId uuid
    Nothing -> fail "Invalid UUID"
  parseJSON _ = fail "Expected string"


instance ToHttpApiData UserId where
  toUrlPiece (UserId uuid) = toText uuid


instance ToJSON UserId where
  toJSON (UserId uuid) = String $ toText uuid


data UserIdentifier
  = UserIdentifierUserId UserId
  | UserIdentifierUsername Text
  deriving (Eq, Show)


instance FromJSON UserIdentifier where
  parseJSON = withText "UserIdentifier" $ \s ->
    case fromText s of
      Just uuid -> return $ UserIdentifierUserId $ UserId uuid
      Nothing -> return $ UserIdentifierUsername s


instance ToJSON UserIdentifier where
  toJSON (UserIdentifierUserId (UserId uuid)) = String $ toText uuid
  toJSON (UserIdentifierUsername username) = String username


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
