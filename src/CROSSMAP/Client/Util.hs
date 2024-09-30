module CROSSMAP.Client.Util
  ( getPassword
  , resolvePassword
  , withEcho
  ) where

import Control.Exception
import Data.Text
import System.IO


getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putStrLn ""
  return pass


withEcho :: Bool -> IO a -> IO a
withEcho echo act = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) act


resolvePassword :: Maybe Text -> IO Text
resolvePassword Nothing = pack <$> getPassword
resolvePassword (Just pass) = return pass
