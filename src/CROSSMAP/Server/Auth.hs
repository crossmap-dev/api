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
import Data.ByteString.Base64
import Data.CaseInsensitive (original)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID, fromText)
import Network.HTTP.Types
import Network.Socket (SockAddr)
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth

import CROSSMAP.Auth
import CROSSMAP.Server.DB
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.State


data SignatureInfo = SignatureInfo
  { signatureInfoHost :: Text
  , signatureInfoRequestId :: UUID
  , signatureInfoPublicKeyInfo :: PublicKeyInfo
  , signatureInfoSocketAddr :: SockAddr
  } deriving (Eq, Show)


data AuthHeaders = AuthHeaders
  { authHeader      :: ByteString
  , hostHeader      :: ByteString
  , publicKeyHeader :: ByteString
  , requestIdHeader :: ByteString
  } deriving (Eq, Show)


type instance AuthServerData (AuthProtect "user-signature") = SignatureInfo


type instance AuthServerData (AuthProtect "session-signature") = SignatureInfo


authContext ::
  State ->
    Context (AuthHandler Request SignatureInfo ': AuthHandler Request SignatureInfo ': '[])
authContext state = userAuthHandler state :. sessionAuthHandler state :. EmptyContext


userAuthHandler :: State -> AuthHandler Request SignatureInfo
userAuthHandler state = mkAuthHandler $ \req -> do
  authHeaders <- ensureCommonAuthHeaders req
  checkAuth state authHeaders req UserKey


sessionAuthHandler :: State -> AuthHandler Request SignatureInfo
sessionAuthHandler state = mkAuthHandler $ \req -> do
  authHeaders <- ensureCommonAuthHeaders req
  checkAuth state authHeaders req SessionKey


ensureCommonAuthHeaders :: Request -> Handler AuthHeaders
ensureCommonAuthHeaders req = do
  authHeader      <- ensureHeader req "Authorization"
  hostHeader      <- ensureHeader req "Host"
  publicKeyHeader <- ensureHeader req "X-CROSSMAP-Public-Key"
  requestIdHeader <- ensureHeader req "X-CROSSMAP-Request-Id"
  liftIO $ putStrLn $ "authHeader: " <> (Data.Text.unpack $ decodeUtf8 authHeader)
  liftIO $ putStrLn $ "hostHeader: " <> (Data.Text.unpack $ decodeUtf8 hostHeader)
  liftIO $ putStrLn $ "publicKeyHeader: " <> (Data.Text.unpack $ decodeUtf8 publicKeyHeader)
  liftIO $ putStrLn $ "requestIdHeader: " <> (Data.Text.unpack $ decodeUtf8 requestIdHeader)
  return AuthHeaders {..}


checkAuth :: State -> AuthHeaders -> Request -> PublicKeyType -> Handler SignatureInfo
checkAuth State{pool=pool} AuthHeaders{..} req keyType = do
  signatureInfoHost <- return $ decodeUtf8 hostHeader
  signatureInfoRequestId <- ensureValidUUID requestIdHeader
  signatureInfoPublicKey <- ensureValidPublicKey publicKeyHeader
  let stringToSign' = stringToSign
        signatureInfoRequestId
        (requestMethod req)
        (encodeUtf8 signatureInfoHost)
        (rawPathInfo req)
        (rawQueryString req)
  liftIO $ putStrLn $ "string to sign: " <> show stringToSign'
  ensureValidSignature authHeader stringToSign' signatureInfoPublicKey
  result <- liftIO $ runQuery pool $ lookupPublicKey signatureInfoPublicKey
  case result of
    Right (Just publicKeyInfo) -> do
      if publicKeyInfoType publicKeyInfo == keyType
        then return SignatureInfo
          { signatureInfoHost = signatureInfoHost
          , signatureInfoRequestId = signatureInfoRequestId
          , signatureInfoPublicKeyInfo = publicKeyInfo
          , signatureInfoSocketAddr = remoteHost req
          }
        else throwError $ err401 { errBody = "Invalid public key type" }
    Right Nothing -> throwError $ err401 { errBody = "Public key not found" }
    Left _ -> throwError $ err500 { errBody = "Database error" }


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
ensureValidPublicKey bs = if Data.ByteString.length bs == 32
  then return $ PublicKey bs
  else throwError $ err401 { errBody = "Invalid public key" }


ensureValidSignature :: ByteString -> ByteString -> PublicKey -> Handler ()
ensureValidSignature authHeader stringToSign' publicKey = do
  -- authHeader is expected to be in the format "Signature <signature>"
  case Data.ByteString.splitAt 10 authHeader of
    ("Signature ", signatureBase64) -> do
      let signature = Data.ByteString.Base64.decodeBase64Lenient signatureBase64
      if Crypto.Sign.Ed25519.dverify publicKey stringToSign' $ Signature signature
        then return ()
        else throwError $ err401 { errBody = "Invalid signature" }
    _ -> throwError $ err401 { errBody = "Invalid Authorization header" }
