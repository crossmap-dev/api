module CROSSMAP.Client.Command.List
  ( ListCommand(..)
  , listOptions
  , runList
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.List.PublicKeys
import CROSSMAP.Client.Command.List.Sessions
import CROSSMAP.Client.Command.List.Users


data ListCommand
  = ListPublicKeys ListPublicKeysCommand
  | ListSessions ListSessionsCommand
  | ListUsers ListUsersCommand
  deriving (Show)


listOptions :: Parser ListCommand
listOptions = hsubparser
  ( command "users"
    ( info (ListUsers <$> listUsersOptions) ( progDesc "List users" ) )
  <> command "sessions"
    ( info (ListSessions <$> listSessionsOptions) ( progDesc "List sessions" ) )
  <> command "public-keys"
    ( info (ListPublicKeys <$> listPublicKeysOptions) ( progDesc "List public keys" ) )
  )


runList :: ListCommand -> IO ()
runList (ListPublicKeys cmd) = runListPublicKeys cmd
runList (ListSessions cmd) = runListSessions cmd
runList (ListUsers cmd) = runListUsers cmd
