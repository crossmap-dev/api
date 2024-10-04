module CROSSMAP.Client.Command.List
  ( ListCommand(..)
  , listOptions
  , runList
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.List.Sessions
import CROSSMAP.Client.Command.List.Users


data ListCommand
  = ListSessions ListSessionsCommand
  | ListUsers ListUsersCommand
  deriving (Show)


listOptions :: Parser ListCommand
listOptions = hsubparser
  ( command "users"
    ( info (ListUsers <$> listUsersOptions) ( progDesc "List users" ) )
  <> command "sessions"
    ( info (ListSessions <$> listSessionsOptions) ( progDesc "List sessions" ) )
  )


runList :: ListCommand -> IO ()
runList (ListSessions cmd) = runListSessions cmd
runList (ListUsers cmd) = runListUsers cmd
