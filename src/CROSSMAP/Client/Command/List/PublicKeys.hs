module CROSSMAP.Client.Command.List.PublicKeys
  ( ListPublicKeysCommand(..)
  , listPublicKeysOptions
  , runListPublicKeys
  ) where

import Data.Text
import Options.Applicative

import CROSSMAP.Base64PublicKey
import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State


data ListPublicKeysCommand = ListPublicKeysCommand deriving (Show)


listPublicKeysOptions :: Parser ListPublicKeysCommand
listPublicKeysOptions = pure ListPublicKeysCommand


runListPublicKeys :: ListPublicKeysCommand -> IO ()
runListPublicKeys ListPublicKeysCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getPublicKeysClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right publicKeys -> do
          putStrLn "Public keys:"
          mapM_ (putStrLn . ("- " <>) . unpack . base64PublicKeyToText) publicKeys
