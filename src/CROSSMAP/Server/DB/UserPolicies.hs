{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.UserPolicies
  ( getUserPolicies
  ) where

import Data.Functor.Contravariant ((>$<))
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.Policy
import CROSSMAP.User


getUserPolicies :: UserId -> Transaction [PolicyId]
getUserPolicies uid = statement uid getUserPoliciesStatement


getUserPoliciesStatement :: Statement UserId [PolicyId]
getUserPoliciesStatement = Statement sql encoder decoder True where
  sql = "SELECT policy_uuid FROM users_policies WHERE user_uuid = $1"
  encoder = unUserId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList (PolicyId <$> D.column (D.nonNullable D.uuid))
