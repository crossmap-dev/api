{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.GroupPolicies
  ( getGroupPoliciesHandler
  , addPolicyToGroupHandler
  , removePolicyFromGroupHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.Group
import CROSSMAP.Policy
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.GroupPolicies
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State


getGroupPoliciesHandler :: State -> SignatureInfo -> GroupId -> Handler [PolicyId]
getGroupPoliciesHandler state@State{..} signatureInfo groupId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runQuery pool $ getGroupPolicies groupId
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right policies -> return policies


addPolicyToGroupHandler ::
  State -> SignatureInfo -> GroupId -> PolicyId -> Handler NoContent
addPolicyToGroupHandler state@State{..} signatureInfo groupId policyId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runUpdate pool $ addPolicyToGroup groupId policyId
  case result of
    Right () -> return NoContent
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }


removePolicyFromGroupHandler ::
  State -> SignatureInfo -> GroupId -> PolicyId -> Handler NoContent
removePolicyFromGroupHandler state@State{..} signatureInfo groupId policyId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runUpdate pool $ removePolicyFromGroup groupId policyId
  case result of
    Right () -> return NoContent
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
