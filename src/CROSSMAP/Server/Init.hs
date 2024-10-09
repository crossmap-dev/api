{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Init
  ( serverInit
  ) where

import Crypto.Sign.Ed25519 (PublicKey)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)

import CROSSMAP.Policy
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Policy
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.DB.UserPolicy
import CROSSMAP.Server.State
import CROSSMAP.User


serverInit :: State -> PublicKey -> IO ()
serverInit State{..} publicKey = do
  result0 <- runQuery pool $ getUserByUsername "admin"
  adminId <- case result0 of
    Left err -> error $ show err
    Right (Just user) -> return user
    Right Nothing -> do
      adminId <- nextRandom
      result1 <- runUpdate pool $ insertUserWithName (UserId adminId) "admin"
      case result1 of
        Left err -> error $ show err
        Right _ -> return (UserId adminId)

  result2 <- runQuery pool $ lookupPublicKey publicKey
  case result2 of
    Left err -> error $ show err
    Right Nothing -> do
      now <- getCurrentTime
      let expires = addUTCTime 31536000 now
      result3 <- runUpdate pool $ insertUserPublicKey adminId now expires publicKey
      case result3 of
        Left err -> error $ show err
        Right _ -> return ()
    Right _ -> return ()

  result4 <- runQuery pool $ getPolicyByName "admin"
  adminPolicyId <- case result4 of
    Left err -> error $ show err
    Right Nothing -> do
      let allowAll = CreatePolicyRule True True "**"
      policy <- createPolicy $ CreatePolicyRequest "admin" [ allowAll ]
      result5 <- runUpdate pool $ insertPolicy policy
      case result5 of
        Left err -> error $ show err
        Right () -> return $ policyId policy
    Right (Just policy) -> return $ policyId policy

  result6 <- runQuery pool $ getUserPolicy adminId adminPolicyId
  case result6 of
    Left err -> error $ show err
    Right Nothing -> do
      result7 <- runUpdate pool $ insertUserPolicy adminId adminPolicyId
      case result7 of
        Left err -> error $ show err
        Right _ -> return ()
    Right _ -> return ()

  putStrLn "Server initialized"
  return ()
