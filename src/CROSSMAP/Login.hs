module CROSSMAP.Login
  ( LoginRequest(..)
  , LoginResponse(..)
  ) where


data LoginRequest = LoginRequest
  { username :: String
  } deriving (Show)


data LoginResponse = LoginResponse
  { session :: String
  } deriving (Show)
