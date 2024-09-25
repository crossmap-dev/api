module CROSSMAP.Server.Command
  ( run
  ) where

import Options.Applicative


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
runWithOptions opts = putStrLn $ "Running with options: " ++ show opts
