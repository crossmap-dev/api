module CROSSMAP.Client.Command
  ( run
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Keypair
import CROSSMAP.Client.Command.Login


data Command
  = Keypair KeypairCommand
  | Login LoginCommand
  deriving (Show)


newtype Options = Options Command deriving (Show)


options :: Parser Options
options = Options <$> hsubparser
  ( command "keypair"
    ( info (Keypair <$> keypairOptions) ( progDesc "Placeholder command" ) )
  <> command "login"
      ( info (Login <$> loginOptions) ( progDesc "Placeholder command" ) )
  )


run :: IO ()
run = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "CROSSMAP cli"
     <> header "crossmap-dev - a CROSSMAP Command Line Interface" )


runWithOptions :: Options -> IO ()
runWithOptions (Options (Keypair cmd)) = runKeypair cmd
runWithOptions (Options (Login cmd)) = runLogin cmd
