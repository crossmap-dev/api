{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Login
  ( LoginCommand(..)
  , loginOptions
  , runLogin
  ) where

import Crypto.Sign.Ed25519
import Data.Text
import Options.Applicative

import CROSSMAP.Password
import CROSSMAP.PublicKey
import CROSSMAP.SecretKey


data LoginCommand = LoginCommand
  { loginCommandUsername :: Text
  , loginCommandPassword :: Text
  } deriving (Show)


loginOptions :: Parser LoginCommand
loginOptions = LoginCommand
  <$> argument str ( metavar "USERNAME" )
  <*> argument str ( metavar "PASSWORD" )


runLogin :: LoginCommand -> IO ()
runLogin LoginCommand{..} = do
  (userPK, userSK) <- keyDerivation loginCommandUsername loginCommandPassword
  (sessionPK, sessionSK) <- createKeypair
  putStrLn $ "User public key: " ++ unpack (publicKeyToText userPK)
  putStrLn $ "User secret key: " ++ unpack (secretKeyToText userSK)
  putStrLn $ "Session public key: " ++ unpack (publicKeyToText sessionPK)
  putStrLn $ "Session secret key: " ++ unpack (secretKeyToText sessionSK)
