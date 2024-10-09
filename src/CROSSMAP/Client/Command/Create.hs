module CROSSMAP.Client.Command.Create
  ( CreateCommand(..)
  , createOptions
  , runCreate
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Create.Group
import CROSSMAP.Client.Command.Create.User


data CreateCommand
  = CreateUser CreateUserCommand
  | CreateGroup CreateGroupCommand
  deriving (Show)


createOptions :: Parser CreateCommand
createOptions = hsubparser
  ( command "user"
    ( info (CreateUser <$> createUserOptions) ( progDesc "Create a user" ) )
  <> command "group"
      ( info (CreateGroup <$> createGroupOptions) ( progDesc "Create a group" ) )
  )


runCreate :: CreateCommand -> IO ()
runCreate (CreateUser cmd) = runCreateUser cmd
runCreate (CreateGroup cmd) = runCreateGroup cmd
