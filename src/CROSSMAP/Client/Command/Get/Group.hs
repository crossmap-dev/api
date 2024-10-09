module CROSSMAP.Client.Command.Get.Group
  ( GetGroupCommand(..)
  , getGroupOptions
  , runGetGroup
  ) where

import Data.Text
import Data.UUID
import Options.Applicative

import CROSSMAP.Client
import CROSSMAP.Client.API
import CROSSMAP.Client.State
import CROSSMAP.Group
import CROSSMAP.User


data GetGroupCommand = GetGroupCommand
  { getGroupCommandGroup :: Text
  } deriving (Show)


getGroupOptions :: Parser GetGroupCommand
getGroupOptions = GetGroupCommand
  <$> argument str ( metavar "GROUP" )


runGetGroup :: GetGroupCommand -> IO ()
runGetGroup (GetGroupCommand groupText) = do
  let maybeGroupId = fromText groupText
  maybeState <- loadState
  case (maybeGroupId, maybeState) of
    (Nothing, _) ->
      putStrLn "Invalid group."
    (_, Nothing) ->
      putStrLn "Client not logged in."
    (Just groupId', Just state) -> do
      client <- loadSessionFromState state
      result <- runClient client $ getGroup (getGroupClientById $ GroupId groupId')
      case result of
        Left err ->
          putStrLn $ "Error: " ++ show err
        Right g -> do
          putStrLn $ "Group: " ++ unpack (toText $ unGroupId $ groupId g)
          putStrLn "Names:"
          mapM_ (putStrLn . ("- " <>) . unpack) (groupNames g)
          putStrLn "Users:"
          mapM_ (putStrLn . ("- " <>) . unpack . toText . unUserId) (groupUsers g)
