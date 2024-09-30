{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Login
  ( LoginCommand(..)
  , loginOptions
  , runLogin
  ) where

import Data.Text
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.State
import CROSSMAP.Client.Util


data LoginCommand = LoginCommand
  { loginCommandURL :: Text
  , loginCommandUsername :: Text
  , loginCommandPassword :: Maybe Text
  } deriving (Show)


loginOptions :: Parser LoginCommand
loginOptions = LoginCommand
  <$> argument str ( metavar "URL" )
  <*> argument str ( metavar "USERNAME" )
  <*> optional (argument str ( metavar "PASSWORD" ))


runLogin :: LoginCommand -> IO ()
runLogin LoginCommand{..} = do
  password <- resolvePassword loginCommandPassword
  client <- newSession loginCommandURL loginCommandUsername password
  saveState $ clientToState client
  putStrLn "Login successful."
