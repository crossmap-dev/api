{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Server.Auth
  ( UserSignature(..)
  , SessionSignature(..)
  , authContext
  ) where

import Control.Monad.IO.Class (liftIO)
import Crypto.Sign.Ed25519
import Data.ByteString
import Data.ByteString.Base64
import Data.CaseInsensitive (original)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID (UUID, fromText)
import Network.HTTP.Types
import Network.Wai
import Servant
import Servant.Server.Experimental.Auth


data UserSignature = UserSignature
  { userSignatureHost :: Text
  , userSignatureRequestId :: UUID
  , userSignaturePublicKey :: PublicKey
  , userSignatureUserId :: UUID
  } deriving (Eq, Show)


data SessionSignature = SessionSignature
  { sessionSignatureHost :: Text
  , sessionSignatureRequestId :: UUID
  , sessionSignaturePublicKey :: PublicKey
  , sessionSignatureSessionId :: UUID
  } deriving (Eq, Show)


data CommonAuthHeaders = CommonAuthHeaders
  { authHeader      :: ByteString
  , hostHeader      :: ByteString
  , publicKeyHeader :: ByteString
  , requestIdHeader :: ByteString
  } deriving (Eq, Show)


type instance AuthServerData (AuthProtect "user-signature") = UserSignature


type instance AuthServerData (AuthProtect "session-signature") = SessionSignature


authContext :: Context
  (AuthHandler Request UserSignature ': AuthHandler Request SessionSignature ': '[])
authContext = userAuthHandler :. sessionAuthHandler :. EmptyContext


userAuthHandler :: AuthHandler Request UserSignature
userAuthHandler = mkAuthHandler $ \req -> do
  authHeaders <- ensureCommonAuthHeaders req
  userIdHeader <- ensureHeader req "X-CROSSMAP-User-Id"
  checkUserAuth authHeaders userIdHeader


sessionAuthHandler :: AuthHandler Request SessionSignature
sessionAuthHandler = mkAuthHandler $ \req -> do
  authHeaders <- ensureCommonAuthHeaders req
  sessionIdHeader <- ensureHeader req "X-CROSSMAP-Session-Id"
  sessionTokenHeader <- ensureHeader req "X-CROSSMAP-Session-Token"
  checkSessionAuth authHeaders sessionIdHeader sessionTokenHeader


ensureCommonAuthHeaders :: Request -> Handler CommonAuthHeaders
ensureCommonAuthHeaders req = do
  authHeader      <- ensureHeader req "Authorization"
  hostHeader      <- ensureHeader req "Host"
  publicKeyHeader <- ensureHeader req "X-CROSSMAP-Public-Key"
  requestIdHeader <- ensureHeader req "X-CROSSMAP-Request-Id"
  return CommonAuthHeaders {..}


checkUserAuth :: CommonAuthHeaders -> ByteString -> Handler UserSignature
checkUserAuth CommonAuthHeaders{..} userId = do
  let stringToSign = hostHeader <> "\n" <> requestIdHeader <> "\n" <> userId
  liftIO $ putStrLn $ "User string to sign: " <> show stringToSign
  userSignatureHost <- return $ decodeUtf8 hostHeader
  userSignatureRequestId <- ensureValidUUID requestIdHeader
  userSignaturePublicKey <- ensureValidPublicKey publicKeyHeader
  userSignatureUserId <- ensureValidUUID userId
  ensureValidSignature authHeader stringToSign userSignaturePublicKey
  return UserSignature{..}


checkSessionAuth ::
  CommonAuthHeaders -> ByteString -> ByteString -> Handler SessionSignature
checkSessionAuth CommonAuthHeaders{..} sessionId sessionToken = do
  let stringToSign = hostHeader <> "\n" <> requestIdHeader
        <> "\n" <> sessionId <> "\n" <> sessionToken
  liftIO $ putStrLn $ "Session string to sign: " <> show stringToSign
  sessionSignatureHost <- return $ decodeUtf8 hostHeader
  sessionSignatureRequestId <- ensureValidUUID requestIdHeader
  sessionSignaturePublicKey <- ensureValidPublicKey publicKeyHeader
  sessionSignatureSessionId <- ensureValidUUID sessionId
  ensureValidSignature authHeader stringToSign sessionSignaturePublicKey
  return SessionSignature{..}


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
ensureValidSignature authHeader stringToSign publicKey = do
  -- authHeader is expected to be in the format "Signature <signature>"
  case Data.ByteString.splitAt 10 authHeader of
    ("Signature ", signatureBase64) -> do
      let signature = Data.ByteString.Base64.decodeBase64Lenient signatureBase64
      if Crypto.Sign.Ed25519.dverify publicKey stringToSign $ Signature signature
        then return ()
        else throwError $ err401 { errBody = "Invalid signature" }
    _ -> throwError $ err401 { errBody = "Invalid Authorization header" }
