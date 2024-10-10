module CROSSMAP.Client.Command.List
  ( ListCommand(..)
  , listOptions
  , runList
  ) where

import Options.Applicative

import CROSSMAP.Client.Command.List.Groups
import CROSSMAP.Client.Command.List.GroupPolicies
import CROSSMAP.Client.Command.List.Policies
import CROSSMAP.Client.Command.List.PublicKeys
import CROSSMAP.Client.Command.List.Sessions
import CROSSMAP.Client.Command.List.Users
import CROSSMAP.Client.Command.List.UserPolicies


data ListCommand
  = ListPolicies ListPoliciesCommand
  | ListPublicKeys ListPublicKeysCommand
  | ListSessions ListSessionsCommand
  | ListUsers ListUsersCommand
  | ListGroups ListGroupsCommand
  | ListUserPolicies ListUserPoliciesCommand
  | ListGroupPolicies ListGroupPoliciesCommand
  deriving (Show)


listOptions :: Parser ListCommand
listOptions = hsubparser
  ( command "users"
    ( info (ListUsers <$> listUsersOptions) ( progDesc "List users" ) )
  <> command "groups"
    ( info (ListGroups <$> listGroupsOptions) ( progDesc "List groups" ) )
  <> command "sessions"
    ( info (ListSessions <$> listSessionsOptions) ( progDesc "List sessions" ) )
  <> command "public-keys"
    ( info (ListPublicKeys <$> listPublicKeysOptions) ( progDesc "List public keys" ) )
  <> command "policies"
    ( info (ListPolicies <$> listPoliciesOptions) ( progDesc "List policies" ) )
  <> command "user-policies"
    ( info (ListUserPolicies <$> listUserPoliciesOptions) ( progDesc "List user policies" ) )
  <> command "group-policies"
    ( info (ListGroupPolicies <$> listGroupPoliciesOptions) ( progDesc "List group policies" ) )
  )


runList :: ListCommand -> IO ()
runList (ListPolicies cmd) = runListPolicies cmd
runList (ListPublicKeys cmd) = runListPublicKeys cmd
runList (ListSessions cmd) = runListSessions cmd
runList (ListUsers cmd) = runListUsers cmd
runList (ListGroups cmd) = runListGroups cmd
runList (ListUserPolicies cmd) = runListUserPolicies cmd
runList (ListGroupPolicies cmd) = runListGroupPolicies cmd
