{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.GroupPolicies
  ( getGroupPolicies
  , addPolicyToGroup
  , removePolicyFromGroup
  ) where

import Data.Functor.Contravariant ((>$<))
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Group
import CROSSMAP.Policy


getGroupPolicies :: GroupId -> Transaction [PolicyId]
getGroupPolicies gid = statement gid getGroupPoliciesStatement


getGroupPoliciesStatement :: Statement GroupId [PolicyId]
getGroupPoliciesStatement = Statement sql encoder decoder True where
  sql = "SELECT policy_uuid FROM groups_policies WHERE group_uuid = $1"
  encoder = unGroupId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList (PolicyId <$> D.column (D.nonNullable D.uuid))


addPolicyToGroup :: GroupId -> PolicyId -> Transaction ()
addPolicyToGroup gid pid = statement (gid, pid) addPolicyToGroupStatement


addPolicyToGroupStatement :: Statement (GroupId, PolicyId) ()
addPolicyToGroupStatement = Statement sql encoder decoder True where
  sql = "INSERT INTO groups_policies (group_uuid, policy_uuid) VALUES ($1, $2)"
  encoder = ((unGroupId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((unPolicyId . snd) >$< E.param (E.nonNullable E.uuid))
  decoder = D.noResult


removePolicyFromGroup :: GroupId -> PolicyId -> Transaction ()
removePolicyFromGroup gid pid = statement (gid, pid) removePolicyFromGroupStatement


removePolicyFromGroupStatement :: Statement (GroupId, PolicyId) ()
removePolicyFromGroupStatement = Statement sql encoder decoder True where
  sql = "DELETE FROM groups_policies WHERE group_uuid = $1 AND policy_uuid = $2"
  encoder = ((unGroupId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((unPolicyId . snd) >$< E.param (E.nonNullable E.uuid))
  decoder = D.noResult
