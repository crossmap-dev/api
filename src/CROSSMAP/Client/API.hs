{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module CROSSMAP.Client.API
  ( GroupClient(..)
  , SessionClient(..)
  , PublicKeyClient(..)
  , PolicyClient(..)
  , loginClient
  , getSessionClient
  , deleteSessionClient
  , getGroupsClient
  , createGroupClient
  , getGroupClientById
  , getGroupPoliciesClient
  , addPolicyToGroupClient
  , removePolicyFromGroupClient
  , getPoliciesClient
  , createPolicyClient
  , getPolicyClientById
  , getPublicKeyClientByPublicKey
  , getPublicKeysClient
  , getUserClient
  , getUsersClient
  , createUserClient
  , createPublicKeyClient
  , getUserByIdClient
  , getUserByUsernameClient
  , getSessionsClient
  , getSessionClientByPublicKey
  ) where

import Data.Text (Text)
import Servant
import Servant.Client

import CROSSMAP.API
import CROSSMAP.Base64PublicKey
import CROSSMAP.Group
import CROSSMAP.Login
import CROSSMAP.Policy
import CROSSMAP.PublicKey
import CROSSMAP.Session
import CROSSMAP.User


type LoginClientM = LoginRequest -> ClientM LoginResponse


type SessionClientM = GetSessionClientM :<|> DeleteSessionClientM


type SessionsClientM = GetSessionsClientM :<|> (Base64PublicKey -> SessionClientM)


type GroupsClientM
  = GetGroupsClientM
  :<|> CreateGroupClientM
  :<|> (GroupId -> GroupClientM)


type GroupClientM = GetGroupClientM :<|> DeleteGroupClientM


type GetGroupsClientM = ClientM [GroupId]


type CreateGroupClientM = CreateGroupRequest -> ClientM Group


type GetGroupClientM = ClientM Group


type DeleteGroupClientM = ClientM NoContent


type GroupPoliciesClientM
  = GetGroupPoliciesClientM
  :<|> AddPolicyToGroupClientM
  :<|> RemovePolicyFromGroupClientM


type GetGroupPoliciesClientM = GroupId -> ClientM [PolicyId]


type AddPolicyToGroupClientM = GroupId -> PolicyId -> ClientM NoContent


type RemovePolicyFromGroupClientM = GroupId -> PolicyId -> ClientM NoContent


type PoliciesClientM
  = GetPoliciesClientM
  :<|> CreatePolicyClientM
  :<|> (PolicyId -> PolicyClientM)


type PolicyClientM = GetPolicyClientM :<|> DeletePolicyClientM


type GetPoliciesClientM = ClientM [PolicyId]


type CreatePolicyClientM = CreatePolicyRequest -> ClientM Policy


type GetPolicyClientM = ClientM Policy


type DeletePolicyClientM = ClientM NoContent


type PublicKeysClientM
  = GetPublicKeysClientM
  :<|> CreatePublicKeyClientM
  :<|> (Base64PublicKey -> PublicKeyClientM)


type GetPublicKeysClientM = ClientM [Base64PublicKey]


type CreatePublicKeyClientM = CreatePublicKeyRequest -> ClientM PublicKeyInfo


type PublicKeyClientM = GetPublicKeyClientM :<|> DeletePublicKeyClientM


type GetPublicKeyClientM = ClientM PublicKeyInfo


type DeletePublicKeyClientM = ClientM NoContent


type UserClientM = GetUserClientM


type UsersClientM
  = GetUsersClientM
  :<|> CreateUserClientM
  :<|> GetUserByIdClientM
  :<|> GetUserByUsernameClientM


type GetSessionClientM = ClientM SessionResponse


type DeleteSessionClientM = ClientM NoContent


type GetUserClientM = ClientM UserResponse


type GetUsersClientM = ClientM [UserId]


type CreateUserClientM = CreateUserRequest -> ClientM UserResponse


type GetUserByIdClientM = UserId -> GetUserClientM


type GetUserByUsernameClientM = Text -> GetUserClientM


type GetSessionsClientM = ClientM [Base64PublicKey]


data GroupClient = GroupClient
  { getGroup :: ClientM Group
  , deleteGroup :: ClientM NoContent
  }


data PolicyClient = PolicyClient
  { getPolicy :: ClientM Policy
  , deletePolicy :: ClientM NoContent
  }


data PublicKeyClient = PublicKeyClient
  { getPublicKey :: ClientM PublicKeyInfo
  , deletePublicKey :: ClientM NoContent
  }


data SessionClient = SessionClient
  { getSession :: GetSessionClientM
  , deleteSession :: DeleteSessionClientM
  }


loginClient :: LoginClientM
loginClient = client loginAPI


sessionClient :: SessionClientM
userClient :: UserClientM
usersClient :: UsersClientM
publicKeysClient :: PublicKeysClientM
sessionsClient :: SessionsClientM
policiesClient :: PoliciesClientM
groupsClient :: GroupsClientM
groupsPoliciesClient :: GroupPoliciesClientM
groupsClient
  :<|> groupsPoliciesClient
  :<|> policiesClient
  :<|> publicKeysClient
  :<|> sessionClient
  :<|> sessionsClient
  :<|> userClient
  :<|> usersClient
  = client privateAPI


getSessionClient :: GetSessionClientM
deleteSessionClient :: DeleteSessionClientM
getSessionClient :<|> deleteSessionClient = sessionClient


getUserClient :: GetUserClientM
getUserClient = userClient


getGroupsClient :: GetGroupsClientM
createGroupClient :: CreateGroupClientM
getGroupClient :: GroupId -> GroupClientM
getGroupsClient :<|> createGroupClient :<|> getGroupClient = groupsClient


getGroupClientById :: GroupId -> GroupClient
getGroupClientById gid =
  let getGroup' :<|> deleteGroup' = getGroupClient gid
   in GroupClient { getGroup = getGroup', deleteGroup = deleteGroup' }


getGroupPoliciesClient :: GetGroupPoliciesClientM
addPolicyToGroupClient :: AddPolicyToGroupClientM
removePolicyFromGroupClient :: RemovePolicyFromGroupClientM
getGroupPoliciesClient :<|> addPolicyToGroupClient :<|> removePolicyFromGroupClient
  = groupsPoliciesClient


getPoliciesClient :: GetPoliciesClientM
createPolicyClient :: CreatePolicyClientM
getPolicyClient :: PolicyId -> PolicyClientM
getPoliciesClient :<|> createPolicyClient :<|> getPolicyClient = policiesClient


getPolicyClientById :: PolicyId -> PolicyClient
getPolicyClientById pid =
  let getPolicy' :<|> deletePolicy' = getPolicyClient pid
   in PolicyClient { getPolicy = getPolicy', deletePolicy = deletePolicy' }


getUsersClient :: GetUsersClientM
createUserClient :: CreateUserClientM
getUserByIdClient :: GetUserByIdClientM
getUserByUsernameClient :: GetUserByUsernameClientM
getUsersClient
  :<|> createUserClient
  :<|> getUserByIdClient
  :<|> getUserByUsernameClient
  = usersClient


getPublicKeysClient :: GetPublicKeysClientM
createPublicKeyClient :: CreatePublicKeyClientM
getPublicKeyClient :: Base64PublicKey -> PublicKeyClientM
getPublicKeysClient :<|> createPublicKeyClient :<|> getPublicKeyClient = publicKeysClient


getPublicKeyClientByPublicKey :: Base64PublicKey -> PublicKeyClient
getPublicKeyClientByPublicKey pk =
  let getPublicKey' :<|> deletePublicKey' = getPublicKeyClient pk
   in PublicKeyClient
        { getPublicKey = getPublicKey'
        , deletePublicKey = deletePublicKey'
        }


getSessionsClient :: GetSessionsClientM
sessionClientByPublicKey :: Base64PublicKey -> SessionClientM
getSessionsClient :<|> sessionClientByPublicKey = sessionsClient


getSessionClientByPublicKey :: Base64PublicKey -> SessionClient
getSessionClientByPublicKey pk =
  let getSession' :<|> deleteSession' = sessionClientByPublicKey pk
   in SessionClient { getSession = getSession', deleteSession = deleteSession' }
