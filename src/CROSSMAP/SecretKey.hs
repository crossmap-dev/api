module CROSSMAP.SecretKey
  ( secretKeyFromText
  , secretKeyToText
  ) where

import Crypto.Sign.Ed25519 (SecretKey(..))
import Data.Base64.Types
import Data.ByteString
import Data.ByteString.Base64
import Data.Text
import Data.Text.Encoding


secretKeyFromText :: Text -> Maybe SecretKey
secretKeyFromText t =
  let bs = decodeBase64Lenient (encodeUtf8 t)
    in if Data.ByteString.length bs == 64
      then Just $ SecretKey bs
      else Nothing


secretKeyToText :: SecretKey -> Text
secretKeyToText (SecretKey bs) = extractBase64 $ encodeBase64 bs
