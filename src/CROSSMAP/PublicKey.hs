{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.PublicKey
  ( Base64PublicKey(..)
  , base64PublicKeyToText
  , publicKeyFromText
  , publicKeyToText
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..))
import Data.Aeson
import Data.Base64.Types
import Data.ByteString
import Data.ByteString.Base64
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Servant


newtype Base64PublicKey = Base64PublicKey
  { unBase64PublicKey :: PublicKey }
  deriving (Eq, Show)


instance FromHttpApiData Base64PublicKey where
  parseUrlPiece t =
    case publicKeyFromText t of
      Just pk -> Right $ Base64PublicKey pk
      Nothing -> Left "Expected a base64 encoded string"


instance FromJSON Base64PublicKey where
  parseJSON (String t) =
    case publicKeyFromText t of
      Just pk -> return $ Base64PublicKey pk
      Nothing -> fail "Expected a base64 encoded string"
  parseJSON _ = fail "Expected a base64 encoded string"


instance ToHttpApiData Base64PublicKey where
  toUrlPiece (Base64PublicKey pk) = publicKeyToText pk


instance ToJSON Base64PublicKey where
  toJSON (Base64PublicKey pk) = String $ publicKeyToText pk


base64PublicKeyToText :: Base64PublicKey -> Text
base64PublicKeyToText (Base64PublicKey pk) = publicKeyToText pk


publicKeyFromText :: Text -> Maybe PublicKey
publicKeyFromText t =
  let bs = decodeBase64Lenient (encodeUtf8 t)
    in if Data.ByteString.length bs == 32
      then Just $ PublicKey bs
      else Nothing


publicKeyToText :: PublicKey -> Text
publicKeyToText (PublicKey bs) = extractBase64 $ encodeBase64 bs
