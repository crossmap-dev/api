{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Config
  ( Env(..)
  , envConfig
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Word (Word16)
import System.Environment (lookupEnv)


data Env = Env
  { envPostgresPool :: Int
  , envPostgresHost :: ByteString
  , envPostgresPort :: Word16
  , envPostgresUser :: ByteString
  , envPostgresPass :: ByteString
  , envPostgresDb   :: ByteString
  } deriving (Show)


envConfig :: IO Env
envConfig = do
  envPostgresPool <- readEnvOrDefault 10 "POSTGRES_POOL"
  envPostgresHost <- loadEnv "POSTGRES_HOST"
  envPostgresPort <- readEnv "POSTGRES_PORT"
  envPostgresUser <- loadEnv "POSTGRES_USER"
  envPostgresPass <- loadEnv "POSTGRES_PASS"
  envPostgresDb   <- loadEnv "POSTGRES_DB"
  return Env{..}


loadEnv :: String -> IO ByteString
loadEnv name = lookupEnv name >>= \case
  Just value -> return $ pack value
  Nothing    -> error $ "Environment variable not set: " ++ name


readEnv :: Read a => String -> IO a
readEnv name = read . unpack <$> loadEnv name


readEnvOrDefault :: Read a => a -> String -> IO a
readEnvOrDefault def name = lookupEnv name >>= \case
  Just value -> return $ read value
  Nothing    -> return def
