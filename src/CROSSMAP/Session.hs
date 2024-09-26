module CROSSMAP.Session
  ( SessionResponse(..)
  ) where


data SessionResponse = SessionResponse
  { session :: String
  } deriving (Show)
