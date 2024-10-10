{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.UserPolicy
  ( getUserPolicy
  , insertUserPolicy
  , deleteUserPolicy
  ) where

import Data.Functor.Contravariant ((>$<))
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Policy
import CROSSMAP.User


getUserPolicy :: UserId -> PolicyId -> Transaction (Maybe (UserId, PolicyId))
getUserPolicy uid pid = statement (uid, pid) getUserPolicyStatement


getUserPolicyStatement :: Statement (UserId, PolicyId) (Maybe (UserId, PolicyId))
getUserPolicyStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid, policy_uuid \
        \FROM users_policies WHERE user_uuid = $1 AND policy_uuid = $2"
  encoder = ((unUserId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((unPolicyId . snd) >$< E.param (E.nonNullable E.uuid))
  decoder = D.rowMaybe $ (,)
        <$> (UserId <$> D.column (D.nonNullable D.uuid))
        <*> (PolicyId <$> D.column (D.nonNullable D.uuid))


insertUserPolicy :: UserId -> PolicyId -> Transaction ()
insertUserPolicy uid pid = statement (uid, pid) insertUserPolicyStatement


insertUserPolicyStatement :: Statement (UserId, PolicyId) ()
insertUserPolicyStatement = Statement sql encoder decoder True where
  sql = "INSERT INTO users_policies (user_uuid, policy_uuid) VALUES ($1, $2)"
  encoder = ((unUserId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((unPolicyId . snd) >$< E.param (E.nonNullable E.uuid))
  decoder = D.noResult


deleteUserPolicy :: UserId -> PolicyId -> Transaction ()
deleteUserPolicy uid pid = statement (uid, pid) deleteUserPolicyStatement


deleteUserPolicyStatement :: Statement (UserId, PolicyId) ()
deleteUserPolicyStatement = Statement sql encoder decoder True where
  sql = "DELETE FROM users_policies WHERE user_uuid = $1 AND policy_uuid = $2"
  encoder = ((unUserId . fst) >$< E.param (E.nonNullable E.uuid))
         <> ((unPolicyId . snd) >$< E.param (E.nonNullable E.uuid))
  decoder = D.noResult
