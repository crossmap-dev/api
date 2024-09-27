module CROSSMAP.Client.Command
  ( run
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Keypair


data Command
  = Keypair KeypairCommand deriving (Show)


newtype Options = Options Command deriving (Show)


options :: Parser Options
options = Options <$> hsubparser
  ( command "keypair"
    ( info (Keypair <$> keypairOptions) ( progDesc "Placeholder command" ) )
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
