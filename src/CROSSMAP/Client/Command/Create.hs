module CROSSMAP.Client.Command.Create
  ( CreateCommand(..)
  , createOptions
  , runCreate
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.Create.Group
import CROSSMAP.Client.Command.Create.GroupPolicy
import CROSSMAP.Client.Command.Create.Policy
import CROSSMAP.Client.Command.Create.User
import CROSSMAP.Client.Command.Create.UserPolicy


data CreateCommand
  = CreateUser CreateUserCommand
  | CreateGroup CreateGroupCommand
  | CreatePolicy CreatePolicyCommand
  | CreateUserPolicy CreateUserPolicyCommand
  | CreateGroupPolicy CreateGroupPolicyCommand
  deriving (Show)


createOptions :: Parser CreateCommand
createOptions = hsubparser
  ( command "user"
    ( info (CreateUser <$> createUserOptions)
      ( progDesc "Create a user" ) )
  <> command "group"
    ( info (CreateGroup <$> createGroupOptions)
      ( progDesc "Create a group" ) )
  <> command "policy"
    ( info (CreatePolicy <$> createPolicyOptions)
      ( progDesc "Create a policy" ) )
  <> command "user-policy"
    ( info (CreateUserPolicy <$> createUserPolicyOptions)
      ( progDesc "Create a user policy" ) )
  <> command "group-policy"
    ( info (CreateGroupPolicy <$> createGroupPolicyOptions)
      ( progDesc "Create a group policy" ) )
  )


runCreate :: CreateCommand -> IO ()
runCreate (CreateUser cmd) = runCreateUser cmd
runCreate (CreateGroup cmd) = runCreateGroup cmd
runCreate (CreatePolicy cmd) = runCreatePolicy cmd
runCreate (CreateUserPolicy cmd) = runCreateUserPolicy cmd
runCreate (CreateGroupPolicy cmd) = runCreateGroupPolicy cmd
