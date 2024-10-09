{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Auth
  ( SignatureInfo(..)
  , authContext
  ) where

import Control.Monad.IO.Class (liftIO)
import Crypto.Sign.Ed25519
import Data.ByteString
import Data.ByteString.Base64.URL
import Data.CaseInsensitive (original)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID, fromText)
import Network.HTTP.Types
import Network.Socket (SockAddr)
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth

import CROSSMAP.Auth
import CROSSMAP.Base64PublicKey
import CROSSMAP.Policy
import CROSSMAP.PublicKey
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.Auth
import CROSSMAP.Server.State


data SignatureInfo = SignatureInfo
  { signatureInfoHost :: Text
  , signatureInfoRequestId :: UUID
  , signatureInfoPolicyIds :: [PolicyId]
  , signatureInfoPublicKeyInfo :: PublicKeyInfo
  , signatureInfoSocketAddr :: SockAddr
  } deriving (Eq, Show)


data AuthHeaders = AuthHeaders
  { authHeader      :: ByteString
  , hostHeader      :: ByteString
  , publicKeyHeader :: ByteString
  , requestIdHeader :: ByteString
  } deriving (Eq, Show)


type instance AuthServerData (AuthProtect "signature") = SignatureInfo


authContext :: State -> Context (AuthHandler Request SignatureInfo ': '[])
authContext state = authHandler state :. EmptyContext


authHandler :: State -> AuthHandler Request SignatureInfo
authHandler state = mkAuthHandler $ \req -> do
  authHeaders <- ensureCommonAuthHeaders req
  checkAuth state authHeaders req


ensureCommonAuthHeaders :: Request -> Handler AuthHeaders
ensureCommonAuthHeaders req = do
  authHeader      <- ensureHeader req "Authorization"
  hostHeader      <- ensureHeader req "Host"
  publicKeyHeader <- ensureHeader req "X-CROSSMAP-Public-Key"
  requestIdHeader <- ensureHeader req "X-CROSSMAP-Request-Id"
  return AuthHeaders {..}


checkAuth :: State -> AuthHeaders -> Request -> Handler SignatureInfo
checkAuth State{pool=pool} AuthHeaders{..} req = do
  signatureInfoHost <- return $ decodeUtf8 hostHeader
  signatureInfoRequestId <- ensureValidUUID requestIdHeader
  signatureInfoPublicKey <- ensureValidPublicKey publicKeyHeader
  let stringToSign' = stringToSign
        signatureInfoRequestId
        (requestMethod req)
        (encodeUtf8 signatureInfoHost)
        (rawPathInfo req)
        (rawQueryString req)
  ensureValidSignature authHeader stringToSign' signatureInfoPublicKey
  result <- liftIO $ runQuery pool $ publicKeyInfoPolicyQuery signatureInfoPublicKey
  case result of
    Right (Just PublicKeyInfoPolicyRow{..}) -> do
      return $ SignatureInfo
        { signatureInfoHost = signatureInfoHost
        , signatureInfoRequestId = signatureInfoRequestId
        , signatureInfoPolicyIds = publicKeyInfoPolicyRowPolicyIds
        , signatureInfoPublicKeyInfo = publicKeyInfoPolicyRowPublicKeyInfo
        , signatureInfoSocketAddr = remoteHost req
        }
    Right Nothing -> throwError $ err401 { errBody = "Public key not found" }
    Left err -> do
      liftIO $ print err
      throwError $ err500 { errBody = "Internal server error" }


ensureHeader :: Request -> HeaderName -> Handler ByteString
ensureHeader req name = case lookup name (requestHeaders req) of
  Just value -> return value
  Nothing -> throwError $ err401
    { errBody = "Missing required header: " <> cast name }
  where cast = fromStrict . original


ensureValidUUID :: ByteString -> Handler UUID
ensureValidUUID bs = case fromText $ decodeUtf8 bs of
  Just uuid -> return uuid
  Nothing -> throwError $ err401 { errBody = "Invalid UUID: " <> fromStrict bs }


ensureValidPublicKey :: ByteString -> Handler PublicKey
ensureValidPublicKey bs = case publicKeyFromText $ decodeUtf8 bs of
  Just publicKey -> return publicKey
  Nothing -> throwError $ err401 { errBody = "Invalid public key: " <> fromStrict bs }


ensureValidSignature :: ByteString -> ByteString -> PublicKey -> Handler ()
ensureValidSignature authHeader stringToSign' publicKey = do
  -- authHeader is expected to be in the format "Signature <signature>"
  case Data.ByteString.splitAt 10 authHeader of
    ("Signature ", signatureBase64) -> do
      let signature = decodeBase64Lenient signatureBase64
      if Crypto.Sign.Ed25519.dverify publicKey stringToSign' $ Signature signature
        then return ()
        else throwError $ err401 { errBody = "Invalid signature" }
    _ -> throwError $ err401 { errBody = "Invalid Authorization header" }
