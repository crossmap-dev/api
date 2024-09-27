module CROSSMAP.Client
  ( Client(..)
  , newSession
  , loadSession
  ) where

import Data.Text
import Crypto.Sign.Ed25519

import CROSSMAP.Password
import CROSSMAP.PublicKey
import CROSSMAP.SecretKey


data Client = Client
  { clientUsername :: Text
  , clientUserPublicKey :: PublicKey
  , clientUserSecretKey :: SecretKey
  , clientSessionPublicKey :: PublicKey
  , clientSessionSecretKey :: SecretKey
  } deriving (Show)


newSession :: Text -> Text -> IO Client
newSession username password = do
  (userPK, userSK) <- keyDerivation username password
  (sessionPK, sessionSK) <- createKeypair
  return Client
    { clientUsername = username
    , clientUserPublicKey = userPK
    , clientUserSecretKey = userSK
    , clientSessionPublicKey = sessionPK
    , clientSessionSecretKey = sessionSK
    }


loadSession :: Text -> Text -> Text -> Text -> Text -> IO Client
loadSession username userPK userSK sessionPK sessionSK = do
  userPK' <- maybe (fail "Invalid user public key") return $ publicKeyFromText userPK
  userSK' <- maybe (fail "Invalid user secret key") return $ secretKeyFromText userSK
  sessionPK' <- maybe (fail "Invalid session public key") return $ publicKeyFromText sessionPK
  sessionSK' <- maybe (fail "Invalid session secret key") return $ secretKeyFromText sessionSK
  return Client
    { clientUsername = username
    , clientUserPublicKey = userPK'
    , clientUserSecretKey = userSK'
    , clientSessionPublicKey = sessionPK'
    , clientSessionSecretKey = sessionSK'
    }
