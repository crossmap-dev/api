{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Group
  ( CreateGroupRequest(..)
  , Group(..)
  , GroupId(..)
  , GroupName
  , createGroup
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.UUID
import Data.UUID.V4
import Servant

import CROSSMAP.User


newtype GroupId = GroupId { unGroupId :: UUID } deriving (Eq, Show)


instance FromHttpApiData GroupId where
  parseUrlPiece s = case fromText s of
    Just uuid -> Right $ GroupId uuid
    Nothing -> Left "Invalid UUID"


instance FromJSON GroupId where
  parseJSON = withText "GroupId" $ \s -> case fromText s of
    Just uuid -> return $ GroupId uuid
    Nothing -> fail "Invalid UUID"


instance ToHttpApiData GroupId where
  toUrlPiece = toText . unGroupId


instance ToJSON GroupId where
  toJSON = String . toText . unGroupId


type GroupName = Text


data CreateGroupRequest = CreateGroupRequest
  { createGroupNames :: [GroupName]
  , createGroupUsers :: [UserIdentifier]
  } deriving (Eq, Show)


instance FromJSON CreateGroupRequest where
  parseJSON = withObject "CreateGroupRequest" $ \o -> do
    createGroupNames <- o .: "names"
    createGroupUsers <- o .: "users"
    return CreateGroupRequest{..}


instance ToJSON CreateGroupRequest where
  toJSON CreateGroupRequest{..} = object
    [ "names" .= createGroupNames
    , "users" .= createGroupUsers
    ]


data Group = Group
  { groupId :: GroupId
  , groupNames :: [GroupName]
  , groupUsers :: [UserId]
  } deriving (Eq, Show)


instance FromJSON Group where
  parseJSON = withObject "Group" $ \o -> do
    groupId <- GroupId <$> o .: "id"
    groupNames <- o .: "names"
    groupUsers <- o .: "users"
    return Group{..}


instance ToJSON Group where
  toJSON Group{..} = object
    [ "id" .= groupId
    , "names" .= groupNames
    , "users" .= groupUsers
    ]


createGroup :: [GroupName] -> [UserId] -> IO Group
createGroup names users = do
  gid <- nextRandom
  return $ Group (GroupId gid) names users
