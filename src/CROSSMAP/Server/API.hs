module CROSSMAP.Server.API
  ( server
  ) where

import Servant

import CROSSMAP.API
import CROSSMAP.Base64PublicKey
import CROSSMAP.Group
import CROSSMAP.Policy
import CROSSMAP.Server.Auth
import CROSSMAP.Server.Handlers
import CROSSMAP.Server.State


server :: State -> Server API
server state = publicServer state :<|> loginServer state :<|> secureServer state


publicServer :: State -> Server PublicAPI
publicServer = indexHandler


loginServer :: State -> Server SecureUserAPI
loginServer = loginHandler


secureServer :: State -> Server SecureSessionAPI
secureServer state sig
  = groupsServer state sig
  :<|> groupPoliciesServer state sig
  :<|> policiesServer state sig
  :<|> publicKeysServer state sig
  :<|> sessionServer state sig
  :<|> sessionsServer state sig
  :<|> userServer state sig
  :<|> usersServer state sig


groupsServer :: State -> SignatureInfo -> Server GroupsAPI
groupsServer state sig
  = getGroupsHandler state sig
  :<|> createGroupHandler state sig
  :<|> groupByIdServer state sig


groupByIdServer :: State -> SignatureInfo -> GroupId -> Server GroupAPI
groupByIdServer state sig pk
  = getGroupHandler state sig pk
  :<|> deleteGroupHandler state sig pk


groupPoliciesServer :: State -> SignatureInfo -> Server GroupPoliciesAPI
groupPoliciesServer state sig
  = getGroupPoliciesHandler state sig
  :<|> addPolicyToGroupHandler state sig
  :<|> removePolicyFromGroupHandler state sig


policiesServer :: State -> SignatureInfo -> Server PoliciesAPI
policiesServer state sig
  = getPoliciesHandler state sig
  :<|> createPolicyHandler state sig
  :<|> policyByIdServer state sig


policyByIdServer :: State -> SignatureInfo -> PolicyId -> Server PolicyAPI
policyByIdServer state sig pk
  = getPolicyHandler state sig pk
  :<|> deletePolicyHandler state sig pk


sessionServer :: State -> SignatureInfo -> Server SessionAPI
sessionServer state sig = getSessionHandler state sig :<|> deleteSessionHandler state sig


userServer :: State -> SignatureInfo -> Server UserAPI
userServer state sig = getUserHandler state sig


usersServer :: State -> SignatureInfo -> Server UsersAPI
usersServer state sig
  = getUsersHandler state sig
  :<|> createUserHandler state sig
  :<|> getUserByIdHandler state sig
  :<|> getUserByUsernameHandler state sig


publicKeysServer :: State -> SignatureInfo -> Server PublicKeysAPI
publicKeysServer state sig
  = getPublicKeysHandler state sig
  :<|> createPublicKeyHandler state sig
  :<|> publicKeyByPublicKeyServer state sig


publicKeyByPublicKeyServer ::
  State -> SignatureInfo -> Base64PublicKey -> Server PublicKeyAPI
publicKeyByPublicKeyServer state sig pk
  = getPublicKeyHandler state sig pk
  :<|> deletePublicKeyHandler state sig pk


sessionsServer :: State -> SignatureInfo -> Server SessionsAPI
sessionsServer state sig
  = getSessionsHandler state sig
  :<|> sessionByPublicKeyServer state sig


sessionByPublicKeyServer :: State -> SignatureInfo -> Base64PublicKey -> Server SessionAPI
sessionByPublicKeyServer state sig pk
  = getSessionByPublicKeyHandler state sig pk
  :<|> deleteSessionByPublicKeyHandler state sig pk
