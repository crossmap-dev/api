module CROSSMAP.Server.DB.CreateUser
  ( createUser
  ) where

import Data.Time.Clock (UTCTime)
import Hasql.Transaction (Transaction)

import CROSSMAP.Base64PublicKey
import CROSSMAP.Server.DB.PublicKey
import CROSSMAP.Server.DB.User
import CROSSMAP.User


createUser ::
  UserId -> UTCTime -> UTCTime -> CreateUserRequest -> Transaction UserResponse
createUser userId now expires req = do
  insertUser userId
  mapM_ insertUsername $ fmap (Username userId) $ createUserRequestUsernames req
  let insertUserPublicKey' = insertUserPublicKey userId now expires . unBase64PublicKey
  mapM_ insertUserPublicKey' $ createUserRequestPublicKeys req
  publicKeys <- listUserPublicKeys userId
  return $ UserResponse (unUserId userId) (createUserRequestUsernames req) publicKeys
