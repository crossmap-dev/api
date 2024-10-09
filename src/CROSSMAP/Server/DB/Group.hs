{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.Group
  ( getGroup
  , getGroups
  , insertGroup
  , deleteGroup
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Group
import CROSSMAP.User (UserId(..))


getGroups :: Transaction [GroupId]
getGroups = statement () getGroupsStatement


getGroupsStatement :: Statement () [GroupId]
getGroupsStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM groups"
  encoder = E.noParams
  decoder = D.rowList (GroupId <$> D.column (D.nonNullable D.uuid))


getGroup :: GroupId -> Transaction (Maybe Group)
getGroup gid = do
  result <- statement gid getGroupStatement
  case result of
    Nothing -> return Nothing
    Just _ -> do
      names <- statement gid getGroupNamesStatement
      users <- statement gid getGroupUsersStatement
      return $ Just $ Group gid names users


getGroupStatement :: Statement GroupId (Maybe GroupId)
getGroupStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM groups WHERE uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowMaybe (GroupId <$> D.column (D.nonNullable D.uuid))


getGroupNamesStatement :: Statement GroupId [Text]
getGroupNamesStatement = Statement sql encoder decoder True where
  sql = "SELECT name FROM groups_names WHERE group_uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList (D.column (D.nonNullable D.text))


getGroupUsersStatement :: Statement GroupId [UserId]
getGroupUsersStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid FROM groups_users WHERE group_uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList (UserId <$> D.column (D.nonNullable D.uuid))


insertGroup :: Group -> Transaction ()
insertGroup (Group gid names users) = do
  statement gid insertGroupStatement
  mapM_ (insertGroupName gid) names
  mapM_ (insertGroupUser gid) users


insertGroupStatement :: Statement GroupId ()
insertGroupStatement = Statement sql encoder decoder True where
  sql = "INSERT INTO groups (uuid) VALUES ($1)"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult


insertGroupName :: GroupId -> Text -> Transaction ()
insertGroupName gid name = statement (gid, name) insertGroupNameStatement


insertGroupNameStatement :: Statement (GroupId, Text) ()
insertGroupNameStatement = Statement sql encoder decoder True where
  sql = "INSERT INTO groups_names (group_uuid, name) VALUES ($1, $2)"
  encoder = ((unGroupId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((snd) >$< E.param (E.nonNullable E.text))
  decoder = D.noResult


insertGroupUser :: GroupId -> UserId -> Transaction ()
insertGroupUser gid uid = statement (gid, uid) insertGroupUserStatement


insertGroupUserStatement :: Statement (GroupId, UserId) ()
insertGroupUserStatement = Statement sql encoder decoder True where
  sql = "INSERT INTO groups_users (group_uuid, user_uuid) VALUES ($1, $2)"
  encoder = ((unGroupId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((unUserId . snd) >$< E.param (E.nonNullable E.uuid))
  decoder = D.noResult


deleteGroup :: GroupId -> Transaction ()
deleteGroup gid = do
  statement gid deleteGroupUsersStatement
  statement gid deleteGroupNamesStatement
  statement gid deleteGroupStatement


deleteGroupUsersStatement :: Statement GroupId ()
deleteGroupUsersStatement = Statement sql encoder decoder True where
  sql = "DELETE FROM groups_users WHERE group_uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult


deleteGroupNamesStatement :: Statement GroupId ()
deleteGroupNamesStatement = Statement sql encoder decoder True where
  sql = "DELETE FROM groups_names WHERE group_uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult


deleteGroupStatement :: Statement GroupId ()
deleteGroupStatement = Statement sql encoder decoder True where
  sql = "DELETE FROM groups WHERE uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult
