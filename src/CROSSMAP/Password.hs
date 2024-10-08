module CROSSMAP.Password
  ( keyDerivation
  ) where

import Crypto.Error
import Crypto.Hash
import Crypto.Sign.Ed25519
import Crypto.KDF.Argon2
import Data.Text
import Data.Text.Encoding


keyDerivation :: Text -> Text -> IO (PublicKey, SecretKey)
keyDerivation username password = do
  let pass = encodeUtf8 password
  let salt = hashWith SHA256 (encodeUtf8 username)
  seed <- throwCryptoErrorIO $ Crypto.KDF.Argon2.hash options pass salt 32
  case createKeypairFromSeed_ seed of
    Nothing -> error "Failed to create keypair"
    Just (pk, sk) -> return (pk, sk)
  where
    options = Options
      { iterations = 2
      , memory = 2 ^ (23 :: Int)
      , parallelism = 4
      , variant = Argon2id
      , version = Version13
      }
