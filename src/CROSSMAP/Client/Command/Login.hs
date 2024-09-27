{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Login
  ( LoginCommand(..)
  , loginOptions
  , runLogin
  ) where

import Data.Text
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.PublicKey


data LoginCommand = LoginCommand
  { loginCommandURL :: Text
  , loginCommandUsername :: Text
  , loginCommandPassword :: Text
  } deriving (Show)


loginOptions :: Parser LoginCommand
loginOptions = LoginCommand
  <$> argument str ( metavar "URL" )
  <*> argument str ( metavar "USERNAME" )
  <*> argument str ( metavar "PASSWORD" )


runLogin :: LoginCommand -> IO ()
runLogin LoginCommand{..} = do
  client <- newSession loginCommandURL loginCommandUsername loginCommandPassword
  putStrLn $ "User public key: " ++ unpack (publicKeyToText (clientUserPublicKey client))
  putStrLn $ "Session public key: " ++ unpack (publicKeyToText (clientSessionPublicKey client))
