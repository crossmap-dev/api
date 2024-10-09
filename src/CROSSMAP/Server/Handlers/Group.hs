{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Handlers.Group
  ( getGroupHandler
  , getGroupsHandler
  , createGroupHandler
  , deleteGroupHandler
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Servant

import CROSSMAP.Group
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Group
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.Auth
import CROSSMAP.Server.Helpers
import CROSSMAP.Server.State


getGroupHandler :: State -> SignatureInfo -> GroupId -> Handler Group
getGroupHandler state@State{..} signatureInfo groupId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runQuery pool $ getGroup groupId
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right Nothing -> throwError err404 { errBody = "Group not found" }
    Right (Just group) -> return group


getGroupsHandler :: State -> SignatureInfo -> Handler [GroupId]
getGroupsHandler state@State{..} signatureInfo = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runQuery pool $ getGroups
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right groups -> return groups


createGroupHandler :: State -> SignatureInfo -> CreateGroupRequest -> Handler Group
createGroupHandler state@State{..} signatureInfo CreateGroupRequest{..} = do
  _ <- authorize state signatureInfo
  result0 <- liftIO $ runQuery pool $ resolveUsers createGroupUsers
  case result0 of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right (Left err) -> throwError err400 { errBody = fromStrict $ encodeUtf8 err }
    Right (Right users) -> do
      group <- liftIO $ createGroup createGroupNames users
      result1 <- liftIO $ runQuery pool $ insertGroup group
      case result1 of
        Right () -> return group
        Left err ->
          liftIO (print err) >> throwError err500 { errBody = "Database error" }


deleteGroupHandler :: State -> SignatureInfo -> GroupId -> Handler NoContent
deleteGroupHandler state@State{..} signatureInfo groupId = do
  _ <- authorize state signatureInfo
  result <- liftIO $ runUpdate pool $ deleteGroup groupId
  case result of
    Left err -> liftIO (print err) >> throwError err500 { errBody = "Database error" }
    Right () -> return NoContent
