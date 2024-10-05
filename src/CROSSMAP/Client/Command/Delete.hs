module CROSSMAP.Client.Command.Delete
  ( DeleteCommand(..)
  , deleteOptions
  , runDelete
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Delete.PublicKey
import CROSSMAP.Client.Command.Delete.Session


data DeleteCommand
  = DeletePublicKey DeletePublicKeyCommand
  | DeleteSession DeleteSessionCommand
  deriving (Show)


deleteOptions :: Parser DeleteCommand
deleteOptions = hsubparser
  ( command "session"
    ( info (DeleteSession <$> deleteSessionOptions) ( progDesc "Delete a session" ) )
  <> command "public-key"
    ( info (DeletePublicKey <$> deletePublicKeyOptions) ( progDesc "Delete a public key" ) )
  )


runDelete :: DeleteCommand -> IO ()
runDelete (DeletePublicKey cmd) = runDeletePublicKey cmd
runDelete (DeleteSession cmd) = runDeleteSession cmd
