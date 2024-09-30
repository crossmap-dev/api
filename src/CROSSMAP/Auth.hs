{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Auth
  ( stringToSign
  ) where

import Data.ByteString
import Data.Text.Encoding
import Data.UUID
import Network.HTTP.Types


stringToSign :: UUID -> Method -> ByteString -> ByteString -> ByteString -> ByteString
stringToSign requestId method host path query =
  (encodeUtf8 $ toText requestId) <> " " <> method <> " " <> host <> path <> query
