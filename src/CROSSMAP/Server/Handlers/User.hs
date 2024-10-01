{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.User
  ( getUserHandler
  , getUserByIdHandler
  , getUserByUsernameHandler
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Servant

import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State
import CROSSMAP.User (UserId(..), UserResponse(..))


getUserHandler :: State -> SignatureInfo -> Handler UserResponse
getUserHandler State{..} SignatureInfo{..} = do
  ensureSession signatureInfoPublicKeyInfo
  let PublicKeyInfo{..} = signatureInfoPublicKeyInfo
  result0 <- liftIO $ runQuery pool $ getUser $ unUserId publicKeyInfoUser
  result1 <- liftIO $ runQuery pool $ getUserUsernames publicKeyInfoUser
  result2 <- liftIO $ runQuery pool $ listUserPublicKeys publicKeyInfoUser
  case (result0, result1, result2) of
    (Right (Just user), Right usernames, Right publicKeys) -> return $ UserResponse
      { userResponseUserId = unUserId user
      , userResponseUsernames = fmap username usernames
      , userResponsePublicKeys = publicKeys
      }
    _ -> throwError err500 { errBody = "Database error" }


getUserByIdHandler :: State -> SignatureInfo -> UserId -> Handler UserResponse
getUserByIdHandler State{..} SignatureInfo{..} userId = do
  ensureSession signatureInfoPublicKeyInfo
  result0 <- liftIO $ runQuery pool $ getUser $ unUserId userId
  result1 <- liftIO $ runQuery pool $ getUserUsernames userId
  result2 <- liftIO $ runQuery pool $ listUserPublicKeys userId
  case (result0, result1, result2) of
    (Right (Just user), Right usernames, Right publicKeys) -> return $ UserResponse
      { userResponseUserId = unUserId user
      , userResponseUsernames = fmap username usernames
      , userResponsePublicKeys = publicKeys
      }
    _ -> throwError err500 { errBody = "Database error" }


getUserByUsernameHandler :: State -> SignatureInfo -> Text -> Handler UserResponse
getUserByUsernameHandler State{..} SignatureInfo{..} name = do
  ensureSession signatureInfoPublicKeyInfo
  result1 <- liftIO $ runQuery pool $ getUserByUsername name
  case result1 of
    Right (Just userId) -> do
      result0 <- liftIO $ runQuery pool $ getUser $ unUserId userId
      result2 <- liftIO $ runQuery pool $ getUserUsernames userId
      result3 <- liftIO $ runQuery pool $ listUserPublicKeys userId
      case (result0, result2, result3) of
        (Right (Just user), Right usernames, Right publicKeys) -> return $ UserResponse
          { userResponseUserId = unUserId user
          , userResponseUsernames = fmap username usernames
          , userResponsePublicKeys = publicKeys
          }
        _ -> throwError err500 { errBody = "Database error" }
    Right Nothing -> throwError err404 { errBody = "User not found" }
    e -> liftIO (print e) >> throwError err500 { errBody = "Database error" }
