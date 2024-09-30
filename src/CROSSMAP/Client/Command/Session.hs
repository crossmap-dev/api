module CROSSMAP.Client.Command.Session
  ( SessionCommand(..)
  , sessionOptions
  , runSession
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.PublicKey
import CROSSMAP.Session


data SessionCommand = SessionCommand deriving (Show)


sessionOptions :: Parser SessionCommand
sessionOptions = pure SessionCommand


runSession :: SessionCommand -> IO ()
runSession SessionCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getSessionClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right s -> do
          let publicKey = sessionResponseSessionPublicKey s
          putStrLn $ "User ID: " ++ unpack (toText (sessionResponseSessionUser s))
          putStrLn $ "Public key: " ++ unpack (base64PublicKeyToText publicKey)
          putStrLn $ "Created at: " ++ show (sessionResponseSessionCreatedAt s)
          putStrLn $ "Expires at: " ++ show (sessionResponseSessionExpiresAt s)
