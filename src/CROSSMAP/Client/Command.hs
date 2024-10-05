module CROSSMAP.Client.Command
  ( run
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Create
import CROSSMAP.Client.Command.Delete
import CROSSMAP.Client.Command.Get
import CROSSMAP.Client.Command.Keypair
import CROSSMAP.Client.Command.List
import CROSSMAP.Client.Command.Login
import CROSSMAP.Client.Command.Logout
import CROSSMAP.Client.Command.Session
import CROSSMAP.Client.Command.User


data Command
  = Create CreateCommand
  | Delete DeleteCommand
  | Get GetCommand
  | Keypair KeypairCommand
  | List ListCommand
  | Login LoginCommand
  | Logout LogoutCommand
  | Session SessionCommand
  | User UserCommand
  deriving (Show)


newtype Options = Options Command deriving (Show)


options :: Parser Options
options = Options <$> hsubparser
  ( command "keypair"
    ( info (Keypair <$> keypairOptions) ( progDesc "Generate a keypair" ) )
  <> command "login"
      ( info (Login <$> loginOptions) ( progDesc "Login" ) )
  <> command "logout"
      ( info (Logout <$> logoutOptions) ( progDesc "Logout" ) )
  <> command "session"
      ( info (Session <$> sessionOptions) ( progDesc "Get the current session" ) )
  <> command "user"
      ( info (User <$> userOptions) ( progDesc "Get the current user" ) )
  <> command "list"
      ( info (List <$> listOptions) ( progDesc "List resources" ) )
  <> command "get"
      ( info (Get <$> getOptions) ( progDesc "Get a resource" ) )
  <> command "create"
      ( info (Create <$> createOptions) ( progDesc "Create a resource" ) )
  <> command "delete"
      ( info (Delete <$> deleteOptions) ( progDesc "Delete a resource" ) )
  )


run :: IO ()
run = execParser opts >>= runWithOptions
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "CROSSMAP cli"
     <> header "crossmap - a CROSSMAP Command Line Interface" )


runWithOptions :: Options -> IO ()
runWithOptions (Options (Keypair cmd)) = runKeypair cmd
runWithOptions (Options (Login cmd)) = runLogin cmd
runWithOptions (Options (Logout cmd)) = runLogout cmd
runWithOptions (Options (Session cmd)) = runSession cmd
runWithOptions (Options (User cmd)) = runUser cmd
runWithOptions (Options (List cmd)) = runList cmd
runWithOptions (Options (Get cmd)) = runGet cmd
runWithOptions (Options (Create cmd)) = runCreate cmd
runWithOptions (Options (Delete cmd)) = runDelete cmd
