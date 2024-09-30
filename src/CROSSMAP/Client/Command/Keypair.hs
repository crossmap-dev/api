{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Keypair
  ( KeypairCommand(..)
  , keypairOptions
  , runKeypair
  ) where

import Crypto.Sign.Ed25519
import Data.Base64.Types
import Data.ByteString.Base64
import Data.Text
import Options.Applicative

import CROSSMAP.Client.Util
import CROSSMAP.Password


data KeypairCommand = KeypairCommand
  { keypairCommandUsername :: Text
  , keypairCommandPassword :: Maybe Text
  } deriving (Show)


keypairOptions :: Parser KeypairCommand
keypairOptions = KeypairCommand
  <$> argument str ( metavar "USERNAME" )
  <*> optional (argument str ( metavar "PASSWORD" ))


runKeypair :: KeypairCommand -> IO ()
runKeypair KeypairCommand{..} = do
  password <- resolvePassword keypairCommandPassword
  (pk, sk) <- keyDerivation keypairCommandUsername password
  putStrLn $ "Public key: " ++ (unpack $ extractBase64 $ encodeBase64 $ unPublicKey pk)
  putStrLn $ "Secret key: " ++ (unpack $ extractBase64 $ encodeBase64 $ unSecretKey sk)
