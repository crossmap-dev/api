module CROSSMAP.Client.Command.Create.User
  ( CreateUserCommand(..)
  , createUserOptions
  , runCreateUser
  ) where

import Data.Text (Text, pack)
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Client.Util
import CROSSMAP.Password
import CROSSMAP.PublicKey
import CROSSMAP.User


data CreateUserCommand = CreateUserCommand
  { createUserName :: Text }
  deriving (Show)


createUserOptions :: Parser CreateUserCommand
createUserOptions = CreateUserCommand
  <$> argument str
      ( metavar "USERNAME"
     <> help "The username to create" )


runCreateUser :: CreateUserCommand -> IO ()
runCreateUser (CreateUserCommand username) = do
  maybeState <- loadState
  case maybeState of
    Nothing ->
      putStrLn "Client not logged in."
    Just state -> do
      client <- loadSessionFromState state
      password <- pack <$> getPasswordWithConfirmation
      (pk, _sk) <- keyDerivation username password
      let createUserReq = CreateUserRequest [username] [Base64PublicKey pk]
      result <- runClient client (createUserClient createUserReq)
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right _ ->
          putStrLn "User created."
