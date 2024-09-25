module CROSSMAP.Client.Command
  ( run
  ) where

import Options.Applicative


data Command = Placeholder deriving (Show)


newtype Options = Options Command deriving (Show)


options :: Parser Options
options = Options <$> hsubparser
  ( command "placeholder"
    ( info ( pure Placeholder ) ( progDesc "Placeholder command" ) )
  )


run :: IO ()
run = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "CROSSMAP cli"
     <> header "crossmap-dev - a CROSSMAP Command Line Interface" )


runWithOptions :: Options -> IO ()
runWithOptions (Options Placeholder) = putStrLn "Placeholder command"
