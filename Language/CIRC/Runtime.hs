-- | The runtime library for CIRC generated code.
module Language.CIRC.Runtime
  ( CIRC
  , evalCIRC
  , runCIRC
  , Id
  , newId
  ) where

import Control.Monad.State

-- | The CIRC transform monad.  Used to create fresh ids.
type CIRC = State Int

-- | Evaluates a CIRC transform.
evalCIRC :: CIRC a -> Int -> a
evalCIRC = evalState

-- | Evaluates a CIRC transform, also returning the fresh next id.
runCIRC :: CIRC a -> Int -> (a, Int)
runCIRC = runState

-- | Identifiers.
type Id = String

-- | Produces a fresh id.
newId :: CIRC Id
newId = do
  id <- get
  put $ id + 1
  return $ "__" ++ show id

