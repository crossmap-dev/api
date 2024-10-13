module CROSSMAP.Password
  ( keyDerivation
  , seedDerivation
  , usernameSalt
  ) where

import Crypto.Error
import Crypto.Hash
import Crypto.Sign.Ed25519
import Crypto.KDF.Argon2
import Data.ByteArray
import Data.ByteString
import Data.Text
import Data.Text.Encoding


keyDerivation :: Text -> Text -> IO (PublicKey, SecretKey)
keyDerivation username password = do
  seed <- seedDerivation username password
  case createKeypairFromSeed_ seed of
    Nothing -> error "Failed to create keypair"
    Just (pk, sk) -> return (pk, sk)


seedDerivation :: Text -> Text -> IO ByteString
seedDerivation username password = do
  let pass = encodeUtf8 password
  let salt = usernameSalt username
  throwCryptoErrorIO $ Crypto.KDF.Argon2.hash options pass salt 32
  where
    options = Options
      { iterations = 4
      , memory = 2 ^ (20 :: Int)
      , parallelism = 1
      , variant = Argon2id
      , version = Version13
      }


usernameSalt :: Text -> ByteString
usernameSalt username =
  Data.ByteArray.take 16 $ Data.ByteArray.convert $ hashWith SHA256 $ encodeUtf8 username
