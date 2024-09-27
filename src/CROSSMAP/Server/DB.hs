module CROSSMAP.Server.DB
  ( connect
  , runQuery
  , runUpdate
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Hasql.Pool as Pool (Pool, UsageError, acquire, use)
import Hasql.Pool.Config as PC (settings, size, staticConnectionSettings)
import Hasql.Connection as C (settings)
import Hasql.Transaction (Transaction)
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)

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


runQuery :: Pool -> Transaction a -> IO (Either UsageError a)
runQuery pool tx = Pool.use pool $ transaction Serializable Read tx


runUpdate :: Pool -> Transaction a -> IO (Either UsageError a)
runUpdate pool tx = Pool.use pool $ transaction Serializable Write tx
