module Typiara.Typ
  ( Typ(..)
  , UnifyResult(..)
  , UnifyError(..)
  ) where

import           Data.Data  (Data)
import           Typiara.FT

-- | User defined type.
class Typ t
  -- TODO: consider dropping `FT` from signatures. The current signature allows a Function to turn into a Nil (for example), which is kinda weird.
  where
  unify ::
       (Eq v, Data v) => FT t v -> FT t v -> Either UnifyError (UnifyResult t v)

-- | Unification may produce side effects, e.g. when two complex types are
-- unified and two different type variables appear at the same index, they have
-- to be merged into one.
data UnifyResult t v =
  UnifyResult
    { unified         :: FT t v
    , typeVarsToUnify :: [(v, v)]
    }
  deriving (Eq, Show, Ord)

data UnifyError =
  ConflictingTypes String String
  deriving (Eq, Show)
