{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.Policies
  ( createPolicyHandler
  , deletePolicyHandler
  , getPolicyHandler
  , getPoliciesHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.Policy
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Policy
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State


createPolicyHandler :: State -> SignatureInfo -> CreatePolicyRequest -> Handler Policy
createPolicyHandler State{..} SignatureInfo{..} req = do
  ensureSession signatureInfoPublicKeyInfo
  policy <- liftIO $ createPolicy req
  result <- liftIO $ runUpdate pool $ insertPolicy policy
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return policy


deletePolicyHandler :: State -> SignatureInfo -> PolicyId -> Handler NoContent
deletePolicyHandler State{..} SignatureInfo{..} policyId = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runUpdate pool $ deletePolicy policyId
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return NoContent


getPolicyHandler :: State -> SignatureInfo -> PolicyId -> Handler Policy
getPolicyHandler State{..} SignatureInfo{..} policyId = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ getPolicyById policyId
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right Nothing -> throwError err404 { errBody = "Policy not found" }
    Right (Just policy) -> return policy


getPoliciesHandler :: State -> SignatureInfo -> Handler [PolicyId]
getPoliciesHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  result <- liftIO $ runQuery pool $ getPolicyIds
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right policyIds -> return policyIds
