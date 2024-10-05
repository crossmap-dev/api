module CROSSMAP.Client.Command.Delete
  ( DeleteCommand(..)
  , deleteOptions
  , runDelete
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Delete.Session


data DeleteCommand
  = DeleteSession DeleteSessionCommand
  deriving (Show)


deleteOptions :: Parser DeleteCommand
deleteOptions = hsubparser
  ( command "session"
    ( info (DeleteSession <$> deleteSessionOptions) ( progDesc "Delete a session" ) )
  )


runDelete :: DeleteCommand -> IO ()
runDelete (DeleteSession cmd) = runDeleteSession cmd
