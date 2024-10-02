module CROSSMAP.Client.Command.Create
  ( CreateCommand(..)
  , createOptions
  , runCreate
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Create.User


data CreateCommand
  = CreateUser CreateUserCommand
  deriving (Show)


createOptions :: Parser CreateCommand
createOptions = hsubparser
  ( command "user"
    ( info (CreateUser <$> createUserOptions) ( progDesc "Create a user" ) )
  )


runCreate :: CreateCommand -> IO ()
runCreate (CreateUser cmd) = runCreateUser cmd
