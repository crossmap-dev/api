module CROSSMAP.Server.State
  ( State(..)
  , new
  ) where

import Hasql.Pool (Pool)


data State = State { pool :: Pool, verbose :: Bool }


new :: Pool -> Bool -> State
new = State
