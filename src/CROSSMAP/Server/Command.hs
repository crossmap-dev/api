{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Command
  ( run
  ) where

import Options.Applicative
import qualified Network.Wai.Handler.Warp as Warp

import CROSSMAP.Server.App
import CROSSMAP.Server.Config
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Migration
import CROSSMAP.Server.Init
import CROSSMAP.Server.State


newtype Options = Options
  { optVerbose :: Bool
  } deriving (Show)


options :: Parser Options
options = Options
  <$> switch
    ( long "verbose"
    <> short 'v'
    <> help "Enable verbose mode" )


run :: IO ()
run = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "CROSSMAP API server"
     <> header "crossmap-dev-server" )


runWithOptions :: Options -> IO ()
runWithOptions opts = envConfig >>= runWithOptionsAndEnv opts


runWithOptionsAndEnv :: Options -> Env -> IO ()
runWithOptionsAndEnv opts env = do
  pool <- connect env
  let state = new pool (optVerbose opts)
  migrate (envMigrationDir env) pool
  serverInit state (envPublicKey env)
  Warp.run (envPort env) (app state)
