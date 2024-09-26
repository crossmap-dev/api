{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Command
  ( run
  ) where

import Options.Applicative

import CROSSMAP.Server.Config
import CROSSMAP.Server.DB
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
     <> progDesc "Crossmap server"
     <> header "crossmap-server - a crossmap server" )


runWithOptions :: Options -> IO ()
runWithOptions opts = envConfig >>= runWithOptionsAndEnv opts


runWithOptionsAndEnv :: Options -> Env -> IO ()
runWithOptionsAndEnv opts env = do
  pool <- connect env
  let state = new pool (optVerbose opts)
  return ()
