{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Init
  ( serverInit
  ) where

import Data.UUID.V4 (nextRandom)

import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.User
import CROSSMAP.Server.State


serverInit :: State -> IO ()
serverInit State{..} = do
  result0 <- runQuery pool $ getUserByUsername "admin"
  adminId <- case result0 of
    Left err -> error $ show err
    Right (Just user) -> return user
    Right Nothing -> do
      adminId <- nextRandom
      result1 <- runUpdate pool $ insertUserWithName (User adminId) "admin"
      case result1 of
        Left err -> error $ show err
        Right _ -> return (User adminId)

  putStrLn "Server initialized"
  return ()
