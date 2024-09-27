module CROSSMAP.PublicKey
  ( Base64PublicKey(..)
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Aeson
import Data.Base64.Types
import Data.ByteString
import Data.ByteString.Base64
import Data.Text.Encoding (encodeUtf8)


newtype Base64PublicKey = Base64PublicKey PublicKey deriving (Eq, Show)


instance FromJSON Base64PublicKey where
  parseJSON (String t) =
    let bs = decodeBase64Lenient (encodeUtf8 t)
      in if Data.ByteString.length bs == 32
        then return $ Base64PublicKey $ PublicKey bs
        else fail "Expected a 32 byte long base64 encoded string"
  parseJSON _ = fail "Expected a base64 encoded string"


instance ToJSON Base64PublicKey where
  toJSON (Base64PublicKey (PublicKey bs)) = String $ extractBase64 $ encodeBase64 bs
