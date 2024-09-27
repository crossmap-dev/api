module CROSSMAP.PublicKey
  ( PublicKeyJSON(..)
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Aeson
import Data.Base64.Types
import Data.ByteString.Base64
import Data.Text.Encoding (encodeUtf8)


newtype PublicKeyJSON = PublicKeyJSON PublicKey deriving (Eq, Show)


instance FromJSON PublicKeyJSON where
  parseJSON (String t) =
    return $ PublicKeyJSON $ PublicKey $ decodeBase64Lenient (encodeUtf8 t)
  parseJSON _ = fail "Expected a base64 encoded string"


instance ToJSON PublicKeyJSON where
  toJSON (PublicKeyJSON (PublicKey bs)) = String $ extractBase64 $ encodeBase64 bs
