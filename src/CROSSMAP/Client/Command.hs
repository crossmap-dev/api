module CROSSMAP.Client.Command
  ( run
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Keypair
import CROSSMAP.Client.Command.Login
import CROSSMAP.Client.Command.Session


data Command
  = Keypair KeypairCommand
  | Login LoginCommand
  | Session SessionCommand
  deriving (Show)


newtype Options = Options Command deriving (Show)


options :: Parser Options
options = Options <$> hsubparser
  ( command "keypair"
    ( info (Keypair <$> keypairOptions) ( progDesc "Generate a keypair" ) )
  <> command "login"
      ( info (Login <$> loginOptions) ( progDesc "Login" ) )
  <> command "session"
      ( info (Session <$> sessionOptions) ( progDesc "Get the current session" ) )
  )


run :: IO ()
run = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "CROSSMAP cli"
     <> header "crossmap - a CROSSMAP Command Line Interface" )


runWithOptions :: Options -> IO ()
runWithOptions (Options (Keypair cmd)) = runKeypair cmd
runWithOptions (Options (Login cmd)) = runLogin cmd
runWithOptions (Options (Session cmd)) = runSession cmd
