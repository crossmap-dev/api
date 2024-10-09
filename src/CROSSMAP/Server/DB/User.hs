{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Server.DB.User
  ( Username(..)
  , getUser
  , getUsers
  , getUserByUsername
  , getUsernames
  , getUserUsernames
  , insertUser
  , insertUsername
  , insertUserWithName
  , userHasName
  , resolveUser
  , resolveUsers
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Text (Text)
import Data.UUID (UUID)
import Hasql.Statement (Statement(..))
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D

import CROSSMAP.User


data Username = Username { usernameUser :: UserId, username :: Text } deriving (Eq, Show)


getUser :: UUID -> Transaction (Maybe UserId)
getUser uuid = statement uuid getUserStatement


getUserStatement :: Statement UUID (Maybe UserId)
getUserStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM users WHERE uuid = $1"
  encoder = E.param (E.nonNullable E.uuid)
  decoder = D.rowMaybe (UserId <$> D.column (D.nonNullable D.uuid))


getUsers :: Transaction [UserId]
getUsers = statement () getUsersStatement


getUsersStatement :: Statement () [UserId]
getUsersStatement = Statement sql encoder decoder True where
  sql = "SELECT uuid FROM users"
  encoder = E.noParams
  decoder = D.rowList (UserId <$> D.column (D.nonNullable D.uuid))


getUserByUsername :: Text -> Transaction (Maybe UserId)
getUserByUsername = flip statement getUserByUsernameStatement


getUserByUsernameStatement :: Statement Text (Maybe UserId)
getUserByUsernameStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid FROM users_names WHERE name = $1"
  encoder = E.param (E.nonNullable E.text)
  decoder = D.rowMaybe (UserId <$> D.column (D.nonNullable D.uuid))


getUsernames :: Transaction [Username]
getUsernames = statement () getUsernamesStatement


getUsernamesStatement :: Statement () [Username]
getUsernamesStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid, name FROM users_names"
  encoder = E.noParams
  decoder = D.rowList userNameDecoder
  userNameDecoder = Username
    <$> (UserId <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.text)


getUserUsernames :: UserId -> Transaction [Username]
getUserUsernames = flip statement getUserUsernamesStatement


getUserUsernamesStatement :: Statement UserId [Username]
getUserUsernamesStatement = Statement sql encoder decoder True where
  sql = "SELECT user_uuid, name FROM users_names WHERE user_uuid = $1"
  encoder = unUserId >$< E.param (E.nonNullable E.uuid)
  decoder = D.rowList userNameDecoder
  userNameDecoder = Username
    <$> (UserId <$> D.column (D.nonNullable D.uuid))
    <*> D.column (D.nonNullable D.text)


insertUser :: UserId -> Transaction ()
insertUser = flip statement insertUserStatement


insertUserStatement :: Statement UserId ()
insertUserStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users (uuid) VALUES ($1)"
  encoder = unUserId >$< E.param (E.nonNullable E.uuid)
  decoder = D.noResult


insertUsername :: Username -> Transaction ()
insertUsername = flip statement insertUsernameStatement


insertUsernameStatement :: Statement Username ()
insertUsernameStatement = Statement sql encoder decoder False where
  sql = "INSERT INTO users_names (user_uuid, name) VALUES ($1, $2)"
  encoder :: E.Params Username
  encoder
    =  ((unUserId . usernameUser) >$< E.param (E.nonNullable E.uuid))
    <> (username >$< E.param (E.nonNullable E.text))
  decoder = D.noResult


insertUserWithName :: UserId -> Text -> Transaction ()
insertUserWithName user name = do
  insertUser user
  insertUsername (Username user name)


userHasName :: UserId -> Text -> Transaction Bool
userHasName user name = do
  names <- getUserUsernames user
  return $ any ((== name) . username) names


resolveUser :: UserIdentifier -> Transaction (Maybe UserId)
resolveUser (UserIdentifierUserId user) = return $ Just user
resolveUser (UserIdentifierUsername name) = getUserByUsername name


resolveUsers :: [UserIdentifier] -> Transaction (Either Text [UserId])
resolveUsers = fmap sequence . mapM resolveUser'
  where
    resolveUser' user = case user of
      UserIdentifierUserId u -> return $ Right u
      UserIdentifierUsername n -> getUserByUsername n >>= \case
        Just u -> return $ Right u
        Nothing -> return $ Left $ "User not found: " <> n
