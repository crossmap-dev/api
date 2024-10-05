{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Config
  ( Env(..)
  , envConfig
  ) where

import Crypto.Sign.Ed25519 (PublicKey)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text (pack)
import Data.Word (Word16)
import System.Environment (lookupEnv)

import CROSSMAP.Base64PublicKey


data Env = Env
  { envPort         :: Int
  , envPostgresPool :: Int
  , envPostgresHost :: ByteString
  , envPostgresPort :: Word16
  , envPostgresUser :: ByteString
  , envPostgresPass :: ByteString
  , envPostgresDb   :: ByteString
  , envMigrationDir :: FilePath
  , envPublicKey    :: PublicKey
  } deriving (Show)


envConfig :: IO Env
envConfig = do
  envPort         <- readEnvOrDefault "PORT" 8080
  envPostgresPool <- readEnvOrDefault "POSTGRES_POOL" 10
  envPostgresHost <- loadEnv "POSTGRES_HOST"
  envPostgresPort <- readEnv "POSTGRES_PORT"
  envPostgresUser <- loadEnv "POSTGRES_USER"
  envPostgresPass <- loadEnv "POSTGRES_PASS"
  envPostgresDb   <- loadEnv "POSTGRES_DB"
  envMigrationDir <- unpack <$> loadEnv "MIGRATION_DIR"
  envPublicKey    <- loadEnvPublicKey "PUBLIC_KEY"
  return Env{..}


loadEnv :: String -> IO ByteString
loadEnv name = lookupEnv name >>= \case
  Just value -> return $ Data.ByteString.Char8.pack value
  Nothing    -> error $ "Environment variable not set: " ++ name


loadEnvPublicKey :: String -> IO PublicKey
loadEnvPublicKey name = lookupEnv name >>= \case
  Nothing -> error $ "Environment variable not set: " ++ name
  Just value -> case publicKeyFromText (Data.Text.pack value) of
    Just pk -> return $ pk
    Nothing -> error $ "Invalid public key: " ++ value


readEnv :: Read a => String -> IO a
readEnv name = read . unpack <$> loadEnv name


readEnvOrDefault :: Read a => String -> a -> IO a
readEnvOrDefault name def = lookupEnv name >>= \case
  Just value -> return $ read value
  Nothing    -> return def
