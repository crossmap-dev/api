{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.User
  ( createUserHandler
  , getUserHandler
  , getUserByIdHandler
  , getUserByUsernameHandler
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Time.Clock
import Data.UUID.V4
import Servant

import CROSSMAP.Server.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.CreateUser
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State
import CROSSMAP.User


createUserHandler :: State -> SignatureInfo -> CreateUserRequest -> Handler UserResponse
createUserHandler state@State{..} signatureInfo req = do
  _ <- authorize state signatureInfo
  userId <- UserId <$> liftIO nextRandom
  now <- liftIO getCurrentTime
  let expire = addUTCTime (60 * 60 * 24 * 30) now
  result0 <- liftIO $ runUpdate pool $ createUser userId now expire req
  case result0 of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right resp -> return resp


getUserHandler :: State -> SignatureInfo -> Handler UserResponse
getUserHandler state signatureInfo@SignatureInfo{..} =
  getUserByIdHandler state signatureInfo $ publicKeyInfoUser signatureInfoPublicKeyInfo


getUserByIdHandler :: State -> SignatureInfo -> UserId -> Handler UserResponse
getUserByIdHandler state@State{..} signatureInfo userId = do
  _ <- authorize state signatureInfo
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
getUserByUsernameHandler state@State{..} signatureInfo name = do
  _ <- authorize state signatureInfo
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
