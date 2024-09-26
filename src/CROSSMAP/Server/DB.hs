module CROSSMAP.Server.DB
  ( connect
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Hasql.Pool (Pool, acquire)
import Hasql.Pool.Config as PC (settings, size, staticConnectionSettings)
import Hasql.Connection as C (settings)

import CROSSMAP.Server.Config (Env(..))


connect :: MonadIO m => Env -> m Pool
connect env = do
  let s = C.settings
        ( envPostgresHost env )
        ( envPostgresPort env )
        ( envPostgresUser env )
        ( envPostgresPass env )
        ( envPostgresDb env )
      c = PC.settings
        [ size ( envPostgresPool env )
        , staticConnectionSettings s
        ]
  pool <- liftIO $ acquire c
  return pool
