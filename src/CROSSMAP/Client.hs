{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Client
  ( Client(..)
  , newSession
  , loadSession
  , runClient
  ) where

import Data.Base64.Types
import Data.ByteString as BS
import Data.ByteString.Base64.URL
import Data.ByteString.Char8 as BC
import Data.Text as T
import Data.Text.Encoding
import Data.UUID
import Data.UUID.V4
import Crypto.Sign.Ed25519
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Servant.Client as SC

import CROSSMAP.Auth
import CROSSMAP.Base64PublicKey
import CROSSMAP.Client.API
import CROSSMAP.Login
import CROSSMAP.Password
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
  url' <- SC.parseBaseUrl $ T.unpack url
  (userPK, userSK) <- keyDerivation username password
  (sessionPK, sessionSK) <- createKeypair
  let uah req = signRequest userPK userSK req
  let sah req = signRequest sessionPK sessionSK req
  loginManager <- newManager tlsManagerSettings { managerModifyRequest = uah }
  let loginEnv = SC.mkClientEnv loginManager url'
  let loginRequest = LoginRequest
        { loginRequestUsername = username
        , loginRequestSessionPublicKey = Base64PublicKey sessionPK
        }
  response <- SC.runClientM (loginClient loginRequest) loginEnv
  case response of
    Left err -> fail $ show err
    Right _ -> do
      manager <- newManager tlsManagerSettings { managerModifyRequest = sah }
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
  url' <- SC.parseBaseUrl $ T.unpack url
  userPK' <- maybe (fail "Invalid user public key") return $ publicKeyFromText userPK
  userSK' <- maybe (fail "Invalid user secret key") return $ secretKeyFromText userSK
  sessionPK' <- maybe (fail "Invalid session public key") return $ publicKeyFromText sessionPK
  sessionSK' <- maybe (fail "Invalid session secret key") return $ secretKeyFromText sessionSK
  let sah req = signRequest sessionPK' sessionSK' req
  manager <- newManager tlsManagerSettings { managerModifyRequest = sah }
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


signRequest :: PublicKey -> SecretKey -> Request -> IO Request
signRequest pk sk req = do
  case lookup "Authorization" $ requestHeaders req of
    Just _ -> return req
    Nothing -> do
      requestId <- nextRandom
      let stringToSign' = authStringToSign req requestId
      let Signature signature = dsign sk $ encodeUtf8 stringToSign'
      let signatureB64 = extractBase64 $ encodeBase64 signature
      let req' = req
            { requestHeaders = requestHeaders req
              <> [ ("X-CROSSMAP-Request-Id", encodeUtf8 $ toText requestId)
                 , ("X-CROSSMAP-Public-Key", encodeUtf8 $ publicKeyToText pk)
                 , ("Authorization", "Signature " <> (encodeUtf8 $ signatureB64))
                 , ("User-Agent", "CROSSMAP Haskell Client" )
                 ]
            }
      return req'


authStringToSign :: Request -> UUID -> Text
authStringToSign req reqId
  = decodeUtf8 $ CROSSMAP.Auth.stringToSign reqId m h p q
  where m = method req
        p = path req
        q = queryString req
        h = hostHeader req


hostHeader :: Request -> ByteString
hostHeader req =
  if secure req
  then if port req == 443
       then host req
       else host req <> ":" <> BC.pack (show $ port req)
  else if port req == 80
       then host req
       else host req <> ":" <> BC.pack (show $ port req)
