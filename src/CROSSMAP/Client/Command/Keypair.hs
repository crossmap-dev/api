{-# LANGUAGE RecordWildCards #-}
module CROSSMAP.Client.Command.Keypair
  ( KeypairCommand(..)
  , keypairOptions
  , runKeypair
  ) where

import Control.Monad
import Crypto.Sign.Ed25519
import Data.Base64.Types
import Data.ByteString.Base64.URL
import Data.Text
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Client.Util
import CROSSMAP.Password
import CROSSMAP.PublicKey
import CROSSMAP.User


data KeypairCommand = KeypairCommand
  { keypairCommandUsername :: Text
  , keypairCommandPassword :: Maybe Text
  , keypairCommandRegister :: Bool
  } deriving (Show)


keypairOptions :: Parser KeypairCommand
keypairOptions = KeypairCommand
  <$> argument str ( metavar "USERNAME" )
  <*> optional (argument str ( metavar "PASSWORD" ))
  <*> switch ( long "register" <> short 'r' <> help "Register the keypair" )


runKeypair :: KeypairCommand -> IO ()
runKeypair KeypairCommand{..} = do
  password <- resolvePassword keypairCommandPassword
  (pk, sk) <- keyDerivation keypairCommandUsername password
  putStrLn $ "Public key: " ++ (unpack $ extractBase64 $ encodeBase64 $ unPublicKey pk)
  putStrLn $ "Secret key: " ++ (unpack $ extractBase64 $ encodeBase64 $ unSecretKey sk)
  when keypairCommandRegister $ do
    maybeState <- loadState
    case maybeState of
      Nothing ->
        putStrLn "Client not logged in."
      Just state -> do
        client <- loadSessionFromState state
        getUserResult <- runClient client getUserClient
        case getUserResult of
          Left err -> do
            putStrLn $ "Error: " ++ show err
            return ()
          Right user -> do
            -- ensure the username is valid
            if (keypairCommandUsername `Prelude.elem` userResponseUsernames user)
              then do
                let req = CreatePublicKeyRequest pk Nothing
                createPublicKeyResult <- runClient client $ createPublicKeyClient req
                case createPublicKeyResult of
                  Left err ->
                    putStrLn $ "Error: " ++ show err
                  Right _ ->
                    putStrLn "Keypair registered."
              else do
                putStrLn $ "Error: " ++ unpack keypairCommandUsername ++ " is not logged in."
                return ()
