module CROSSMAP.Client.Command.Get.PublicKey
  ( GetPublicKeyCommand(..)
  , getPublicKeyOptions
  , runGetPublicKey
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Base64PublicKey
import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.PublicKey
import CROSSMAP.User


data GetPublicKeyCommand = GetPublicKeyCommand
  { getPublicKeyCommandPublicKey :: Text
  } deriving (Show)


getPublicKeyOptions :: Parser GetPublicKeyCommand
getPublicKeyOptions = GetPublicKeyCommand
  <$> argument str ( metavar "PUBLIC_KEY" )


runGetPublicKey :: GetPublicKeyCommand -> IO ()
runGetPublicKey (GetPublicKeyCommand publicKeyText) = do
  let maybePublicKey = publicKeyFromText publicKeyText
  maybeState <- loadState
  case (maybePublicKey, maybeState) of
    (Nothing, _) ->
      putStrLn "Invalid public key."
    (_, Nothing) ->
      putStrLn "Client not logged in."
    (Just publicKey, Just state) -> do
      client <- loadSessionFromState state
      let publicKeyClient = getPublicKeyClientByPublicKey $ Base64PublicKey publicKey
      result <- runClient client $ getPublicKey publicKeyClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right publicKeyInfo -> do
          let pk = Base64PublicKey $ publicKeyInfoPublicKey publicKeyInfo
          let UserId uid = publicKeyInfoUser publicKeyInfo
          putStrLn $ "Public key: " ++ unpack (base64PublicKeyToText pk)
          putStrLn $ "Public key type: " ++ show (publicKeyInfoType publicKeyInfo)
          putStrLn $ "Public key user: " ++ unpack (toText uid)
          putStrLn $ "Public key created at: " ++ show (publicKeyInfoCreated publicKeyInfo)
          putStrLn $ "Public key expires at: " ++ show (publicKeyInfoExpires publicKeyInfo)
