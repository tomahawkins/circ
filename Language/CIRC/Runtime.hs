-- | The runtime library for CIRC generated code.
module Language.CIRC.Runtime
  ( CIRC
  , evalCIRC
  , runCIRC
  , Id
  , newId
  , idId
  ) where

import Control.Monad.State

-- | The CIRC transform monad.  Used to create fresh ids.
type CIRC = State (Int, [(String, Int)])

-- | Evaluates a CIRC transform.
evalCIRC :: CIRC a -> Int -> a
evalCIRC a i = evalState a (i, [])

-- | Evaluates a CIRC transform, also returning the fresh next id.
runCIRC :: CIRC a -> Int -> (a, Int)
runCIRC a i = (b, j)
  where
  (b, (j, _)) = runState a (i, [])

-- | Identifiers.
type Id = String

-- | Produces a fresh id.
newId :: CIRC Id
newId = do
  (i, table) <- get
  put (i + 1, table)
  return $ "__" ++ show i

-- | Returns a unqiue int for a given id.
idId  :: Id -> CIRC Int
idId name = do
  (i, table) <- get
  case lookup name table of
    Nothing -> do
      put (i + 1, (name, i) : table)
      return i
    Just i -> return i

