module Typiara.Infer.Typ (Typ(..), UnifyResult(..), UnifyError(..)) where

import Typiara.Infer.FT

-- | User defined type.
class Typ t
  -- TODO: consider dropping `FT` from signatures. The current signature allows a Function to turn into a Nil (for example), which is kinda weird.
  where
  unify ::
       (Eq v) => FT t v -> FT t v -> Either (UnifyError t v) (UnifyResult t v)

-- | Unification may produce side effects, e.g. when two complex types are
-- unified and two different type variables appear at the same index, they have
-- to be merged into one.
data UnifyResult t v
  = Unified (FT t v)
  | TypeVarsToUnify [(v, v)]
  deriving (Eq, Show, Ord)

data UnifyError t v =
  ConflictingTypes (FT t v) (FT t v)
  deriving (Eq, Show)
