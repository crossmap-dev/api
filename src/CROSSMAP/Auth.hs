{-# LANGUAGE OverloadedStrings #-}
module CROSSMAP.Auth
  ( stringToSign
  ) where

import Data.ByteString
import Data.Text.Encoding
import Data.UUID
import Network.HTTP.Types


stringToSign :: UUID -> Method -> ByteString -> ByteString -> ByteString -> ByteString
stringToSign uuid method host path query =
  (encodeUtf8 $ toText uuid) <> " " <> method <> " " <> host <> path <> query
