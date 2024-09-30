{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Login
  ( LoginCommand(..)
  , loginOptions
  , runLogin
  ) where

import Control.Exception
import Data.Text
import Options.Applicative
import System.IO

import CROSSMAP.Client
import CROSSMAP.Client.State


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


resolvePassword :: Maybe Text -> IO Text
resolvePassword Nothing = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putStrLn ""
  return $ pack pass
resolvePassword (Just pass) = return pass


withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) act
