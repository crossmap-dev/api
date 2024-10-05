module CROSSMAP.Client.Command.Delete.PublicKey
  ( DeletePublicKeyCommand(..)
  , deletePublicKeyOptions
  , runDeletePublicKey
  ) where

import Data.Text (Text)
import Options.Applicative

import CROSSMAP.Base64PublicKey
import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State


newtype DeletePublicKeyCommand = DeletePublicKeyCommand
  { publicKeyText :: Text }
  deriving (Show)


deletePublicKeyOptions :: Parser DeletePublicKeyCommand
deletePublicKeyOptions = DeletePublicKeyCommand
  <$> argument str
      ( metavar "PUBLIC_KEY"
     <> help "The public key to delete" )


runDeletePublicKey :: DeletePublicKeyCommand -> IO ()
runDeletePublicKey (DeletePublicKeyCommand pkt) = do
  maybeState <- loadState
  case (maybeState, publicKeyFromText pkt) of
    (Nothing, _) ->
      putStrLn "Client not logged in."
    (_, Nothing) ->
      putStrLn "Invalid public key."
    (Just state, Just publicKey) -> do
      client <- loadSessionFromState state
      let publicKeyClient = getPublicKeyClientByPublicKey $ Base64PublicKey publicKey
      result <- runClient client $ deletePublicKey publicKeyClient
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "Public key deleted."
