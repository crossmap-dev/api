module CROSSMAP.Client.Command.Get
  ( GetCommand(..)
  , getOptions
  , runGet
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Get.PublicKey
import CROSSMAP.Client.Command.Get.Session
import CROSSMAP.Client.Command.Get.User


data GetCommand
  = GetPublicKey GetPublicKeyCommand
  | GetSession GetSessionCommand
  | GetUser GetUserCommand
  deriving (Show)


getOptions :: Parser GetCommand
getOptions = hsubparser
  ( command "user"
    ( info (GetUser <$> getUserOptions) ( progDesc "Get user" ) )
  <> command "session"
    ( info (GetSession <$> getSessionOptions) ( progDesc "Get session" ) )
  <> command "public-key"
    ( info (GetPublicKey <$> getPublicKeyOptions) ( progDesc "Get public key" ) )
  )


runGet :: GetCommand -> IO ()
runGet (GetUser cmd) = runGetUser cmd
runGet (GetSession cmd) = runGetSession cmd
runGet (GetPublicKey cmd) = runGetPublicKey cmd
