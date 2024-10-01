module CROSSMAP.Client.Command.Get.User
  ( GetUserCommand(..)
  , getUserOptions
  , runGetUser
  ) where

import Data.Text (Text)
import Options.Applicative


data GetUserCommand = GetUserCommand
  { getUserCommandUsername :: Text
  } deriving (Show)


getUserOptions :: Parser GetUserCommand
getUserOptions = GetUserCommand
  <$> argument str ( metavar "USERNAME" )


runGetUser :: GetUserCommand -> IO ()
runGetUser (GetUserCommand username) = do
  putStrLn $ "Getting user: " ++ show username
