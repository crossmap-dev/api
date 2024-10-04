module CROSSMAP.Client.Command.Get
  ( GetCommand(..)
  , getOptions
  , runGet
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Get.Session
import CROSSMAP.Client.Command.Get.User


data GetCommand
  = GetSession GetSessionCommand
  | GetUser GetUserCommand
  deriving (Show)


getOptions :: Parser GetCommand
getOptions = hsubparser
  ( command "user"
    ( info (GetUser <$> getUserOptions) ( progDesc "Get user" ) )
  <> command "session"
    ( info (GetSession <$> getSessionOptions) ( progDesc "Get session" ) )
  )


runGet :: GetCommand -> IO ()
runGet (GetSession cmd) = runGetSession cmd
runGet (GetUser cmd) = runGetUser cmd
