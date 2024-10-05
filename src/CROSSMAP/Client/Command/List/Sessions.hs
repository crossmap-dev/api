module CROSSMAP.Client.Command.List.Sessions
  ( ListSessionsCommand(..)
  , listSessionsOptions
  , runListSessions
  ) where

import Data.Text
import Options.Applicative

import CROSSMAP.Base64PublicKey
import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State


data ListSessionsCommand = ListSessionsCommand deriving (Show)


listSessionsOptions :: Parser ListSessionsCommand
listSessionsOptions = pure ListSessionsCommand


runListSessions :: ListSessionsCommand -> IO ()
runListSessions ListSessionsCommand = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      result <- runClient client getSessionsClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right sessions -> do
          putStrLn "Sessions:"
          mapM_ (putStrLn . ("- " <>) . unpack . base64PublicKeyToText) sessions
