module CROSSMAP.Client.Command.Get
  ( GetCommand(..)
  , getOptions
  , runGet
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Get.User


data GetCommand
  = GetUser GetUserCommand
  deriving (Show)


getOptions :: Parser GetCommand
getOptions = hsubparser
  ( command "user"
    ( info (GetUser <$> getUserOptions) ( progDesc "Get user" ) )
  )


runGet :: GetCommand -> IO ()
runGet (GetUser cmd) = runGetUser cmd
