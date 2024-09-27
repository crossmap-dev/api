module CROSSMAP.Password
  (
  ) where

import Crypto.Sign.Ed25519 (PublicKey(..), SecretKey(..))
import Data.Text


data LoginDetails = LoginDetails
  { username :: Text
  , password :: Text
  }
