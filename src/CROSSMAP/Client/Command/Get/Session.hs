module CROSSMAP.Client.Command.Get.Session
  ( GetSessionCommand(..)
  , getSessionOptions
  , runGetSession
  ) where

import Data.Text (Text, unpack)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Base64PublicKey
import CROSSMAP.Session


data GetSessionCommand = GetSessionCommand
  { getSessionCommandSession :: Text
  } deriving (Show)


getSessionOptions :: Parser GetSessionCommand
getSessionOptions = GetSessionCommand
  <$> argument str ( metavar "SESSION" )


runGetSession :: GetSessionCommand -> IO ()
runGetSession (GetSessionCommand publicKeyText) = do
  let maybePublicKey = publicKeyFromText publicKeyText
  maybeState <- loadState
  case (maybePublicKey, maybeState) of
    (Nothing, _) ->
      putStrLn "Invalid public key."
    (_, Nothing) ->
      putStrLn "Client not logged in."
    (Just publicKey, Just state) -> do
      client <- loadSessionFromState state
      let sessionClient = getSessionClientByPublicKey $ Base64PublicKey publicKey
      result <- runClient client $ getSession sessionClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right session -> do
          putStrLn $ "Session user: " ++ show (sessionResponseSessionUser session)
          putStrLn $ "Session public key: " ++ unpack (base64PublicKeyToText (sessionResponseSessionPublicKey session))
          putStrLn $ "Session created at: " ++ show (sessionResponseSessionCreatedAt session)
          putStrLn $ "Session expires at: " ++ show (sessionResponseSessionExpiresAt session)
