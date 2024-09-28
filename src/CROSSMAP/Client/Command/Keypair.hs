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

import CROSSMAP.Password


data KeypairCommand = KeypairCommand
  { keypairCommandUsername :: Text
  , keypairCommandPassword :: Text
  } deriving (Show)


keypairOptions :: Parser KeypairCommand
keypairOptions = KeypairCommand
  <$> argument str ( metavar "USERNAME" )
  <*> argument str ( metavar "PASSWORD" )


runKeypair :: KeypairCommand -> IO ()
runKeypair KeypairCommand{..} = do
  (pk, sk) <- keyDerivation keypairCommandUsername keypairCommandPassword
  putStrLn $ "Public key: " ++ (unpack $ extractBase64 $ encodeBase64 $ unPublicKey pk)
  putStrLn $ "Secret key: " ++ (unpack $ extractBase64 $ encodeBase64 $ unSecretKey sk)
