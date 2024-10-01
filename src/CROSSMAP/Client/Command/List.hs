module CROSSMAP.Client.Command.List
  ( ListCommand(..)
  , listOptions
  , runList
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.List.Users


data ListCommand
  = ListUsers ListUsersCommand
  deriving (Show)


listOptions :: Parser ListCommand
listOptions = hsubparser
  ( command "users"
    ( info (ListUsers <$> listUsersOptions) ( progDesc "List users" ) )
  )


runList :: ListCommand -> IO ()
runList (ListUsers cmd) = runListUsers cmd
