module CROSSMAP.Client.Command.Delete.Session
  ( DeleteSessionCommand(..)
  , deleteSessionOptions
  , runDeleteSession
  ) where

import Data.Text (Text)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.PublicKey


newtype DeleteSessionCommand = DeleteSessionCommand
  { sessionPublicKeyText :: Text }
  deriving (Show)


deleteSessionOptions :: Parser DeleteSessionCommand
deleteSessionOptions = DeleteSessionCommand
  <$> argument str
      ( metavar "SESSION"
     <> help "The session to delete" )


runDeleteSession :: DeleteSessionCommand -> IO ()
runDeleteSession (DeleteSessionCommand pkt) = do
  maybeState <- loadState
  case (maybeState, publicKeyFromText pkt) of
    (Nothing, _) ->
      putStrLn "Client not logged in."
    (_, Nothing) ->
      putStrLn "Invalid session."
    (Just state, Just sessionPublicKey) -> do
      client <- loadSessionFromState state
      let sessionClient = getSessionClientByPublicKey $ Base64PublicKey sessionPublicKey
      result <- runClient client $ deleteSession sessionClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "Session deleted."

