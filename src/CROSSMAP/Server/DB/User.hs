{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.User
  ( User(..)
  , Username(..)
  , getUser
  , getUsers
  , getUserByUsername
  , getUsernames
  , getUserUsernames
  , insertUser
  , insertUsername
  , insertUserWithName
  , userHasName
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.UUID (UUID)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D


newtype User = User { userId :: UUID } deriving (Eq, Show)


data Username = Username { usernameUser :: User, username :: Text } deriving (Eq, Show)


getUser :: UUID -> Transaction (Maybe User)
getUser uuid = statement uuid getUserStatement


getUserStatement :: Statement UUID (Maybe User)
getUserStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM users WHERE uuid = $1"
  encoder = E.param (E.nonNullable E.uuid)
  decoder = D.rowMaybe (User <$> D.column (D.nonNullable D.uuid))


getUsers :: Transaction [User]
getUsers = statement () getUsersStatement


getUsersStatement :: Statement () [User]
getUsersStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM users"
  encoder = E.noParams
  decoder = D.rowList (User <$> D.column (D.nonNullable D.uuid))


getUserByUsername :: Text -> Transaction (Maybe User)
getUserByUsername = flip statement getUserByUsernameStatement


getUserByUsernameStatement :: Statement Text (Maybe User)
getUserByUsernameStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid FROM users_names WHERE name = $1"
  encoder = E.param (E.nonNullable E.text)
  decoder = D.rowMaybe (User <$> D.column (D.nonNullable D.uuid))


getUsernames :: Transaction [Username]
getUsernames = statement () getUsernamesStatement


getUsernamesStatement :: Statement () [Username]
getUsernamesStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid, name FROM users_names"
  encoder = E.noParams
  decoder = D.rowList userNameDecoder
  userNameDecoder = Username
    <$> (User <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.text)


getUserUsernames :: User -> Transaction [Username]
getUserUsernames = flip statement getUserUsernamesStatement


getUserUsernamesStatement :: Statement User [Username]
getUserUsernamesStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid, name FROM users_names WHERE user_uuid = $1"
  encoder = userId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList userNameDecoder
  userNameDecoder = Username
    <$> (User <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.text)


insertUser :: User -> Transaction ()
insertUser = flip statement insertUserStatement


insertUserStatement :: Statement User ()
insertUserStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users (uuid) VALUES ($1)"
  encoder = userId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult


insertUsername :: Username -> Transaction ()
insertUsername = flip statement insertUsernameStatement


insertUsernameStatement :: Statement Username ()
insertUsernameStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users_names (user_uuid, name) VALUES ($1, $2)"
  encoder :: E.Params Username
  encoder
    =  ((userId . usernameUser) >$< E.param (E.nonNullable E.uuid))
    <> (username >$< E.param (E.nonNullable E.text))
  decoder = D.noResult


insertUserWithName :: User -> Text -> Transaction ()
insertUserWithName user name = do
  insertUser user
  insertUsername (Username user name)


userHasName :: User -> Text -> Transaction Bool
userHasName user name = do
  names <- getUserUsernames user
  return $ any ((== name) . username) names
