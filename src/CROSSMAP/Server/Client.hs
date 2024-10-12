module CROSSMAP.Server.Client
  ( Client(..)
  , ClientId(..)
  ) where

import Data.UUID


data Client = Client
  { clientId :: ClientId
  } deriving (Eq, Show)


newtype ClientId = ClientId
  { unClientId :: UUID
  } deriving (Eq, Show)
