module CROSSMAP.Client.Util
  ( getPassword
  , getPasswordWithPrompt
  , getPasswordWithConfirmation
  , resolvePassword
  , withEcho
  ) where

import Control.Exception
import Data.Text
import System.IO


getPassword :: IO String
getPassword = getPasswordWithPrompt "Password: "


getPasswordWithPrompt :: String -> IO String
getPasswordWithPrompt prompt = do
  putStr prompt
  hFlush stdout
  pass <- withEcho False getLine
  putStrLn ""
  return pass


getPasswordWithConfirmation :: IO String
getPasswordWithConfirmation = do
  pass1 <- getPassword
  pass2 <- getPasswordWithPrompt "Confirm password: "
  if pass1 == pass2
    then return pass1
    else do
      putStrLn "Passwords do not match."
      getPasswordWithConfirmation


withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) act


resolvePassword :: Maybe Text -> IO Text
resolvePassword Nothing = pack <$> getPassword
resolvePassword (Just pass) = return pass
