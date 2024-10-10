{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.UserPolicies
  ( getUserPoliciesHandler
  , addUserPolicyHandler
  , removeUserPolicyHandler
  ) where

import Control.Monad.IO.Class
import Servant

import CROSSMAP.Policy
import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.UserPolicies
import CROSSMAP.Server.DB.UserPolicy
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State
import CROSSMAP.User


getUserPoliciesHandler :: State -> SignatureInfo -> UserId -> Handler [PolicyId]
getUserPoliciesHandler state@State{..} signatureInfo userId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runQuery pool $ getUserPolicies userId
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right policies -> return policies


addUserPolicyHandler ::
  State -> SignatureInfo -> UserId -> PolicyId -> Handler NoContent
addUserPolicyHandler state@State{..} signatureInfo userId policyId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runUpdate pool $ insertUserPolicy userId policyId
  case result of
    Right () -> return NoContent
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }


removeUserPolicyHandler ::
  State -> SignatureInfo -> UserId -> PolicyId -> Handler NoContent
removeUserPolicyHandler state@State{..} signatureInfo userId policyId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runUpdate pool $ deleteUserPolicy userId policyId
  case result of
    Right () -> return NoContent
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
