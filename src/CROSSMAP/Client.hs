module CROSSMAP.Client
  ( Client(..)
  , newSession
  , loadSession
  , runClient
  ) where

import Data.Text
import Crypto.Sign.Ed25519
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Servant.Client as SC

import CROSSMAP.Password
import CROSSMAP.PublicKey
import CROSSMAP.SecretKey


type ClientM = SC.ClientM


type ClientError = SC.ClientError


data Client = Client
  { clientURL :: Text
  , clientEnv :: SC.ClientEnv
  , clientManager :: Manager
  , clientUsername :: Text
  , clientUserPublicKey :: PublicKey
  , clientUserSecretKey :: SecretKey
  , clientSessionPublicKey :: PublicKey
  , clientSessionSecretKey :: SecretKey
  }


newSession :: Text -> Text -> Text -> IO Client
newSession url username password = do
  (userPK, userSK) <- keyDerivation username password
  (sessionPK, sessionSK) <- createKeypair
  manager <- newManager tlsManagerSettings
  url' <- SC.parseBaseUrl $ unpack url
  return Client
    { clientURL = url
    , clientEnv = SC.mkClientEnv manager url'
    , clientManager = manager
    , clientUsername = username
    , clientUserPublicKey = userPK
    , clientUserSecretKey = userSK
    , clientSessionPublicKey = sessionPK
    , clientSessionSecretKey = sessionSK
    }


loadSession :: Text -> Text -> Text -> Text -> Text -> Text -> IO Client
loadSession url username userPK userSK sessionPK sessionSK = do
  url' <- SC.parseBaseUrl $ unpack url
  manager <- newManager tlsManagerSettings
  userPK' <- maybe (fail "Invalid user public key") return $ publicKeyFromText userPK
  userSK' <- maybe (fail "Invalid user secret key") return $ secretKeyFromText userSK
  sessionPK' <- maybe (fail "Invalid session public key") return $ publicKeyFromText sessionPK
  sessionSK' <- maybe (fail "Invalid session secret key") return $ secretKeyFromText sessionSK
  return Client
    { clientURL = url
    , clientEnv = SC.mkClientEnv manager url'
    , clientManager = manager
    , clientUsername = username
    , clientUserPublicKey = userPK'
    , clientUserSecretKey = userSK'
    , clientSessionPublicKey = sessionPK'
    , clientSessionSecretKey = sessionSK'
    }


runClient :: Client -> ClientM a -> IO (Either ClientError a)
runClient client action = SC.runClientM action $ clientEnv client
