module Typiara.Typ
  ( Typ(..)
  , UnifyResult(..)
  , UnifyError(..)
  , unifyEq
  ) where

import           Data.Data           (Data)
import           Typiara.Data.Tagged

-- | User defined type.
--
-- Represents only non-function types. Functions are built into the library, in the `FT` module.
-- For a type system that operates on integers and strings (and functions on those types), this type
-- should only define variants for an integer and a string, while the library takes care of
-- function types.
--
-- `unify` defines how leaf types merge together.
class Typ t where
  unify :: (Eq v, Data v) => t v -> t v -> Either UnifyError (UnifyResult t v)

unifyEq :: (Typ t, Eq (t a)) => t a -> t a -> Maybe (t a)
unifyEq x y =
  if x == y
    then Just x
    else Nothing

-- | Unification may produce side effects, e.g. when two complex types are
-- unified and two different type variables appear at the same index, they have
-- to be merged into one.
data UnifyResult t v =
  UnifyResult
    { unified         :: t v
    , typeVarsToUnify :: [(v, v)]
    }
  deriving (Eq, Show, Ord)

data UnifyError =
  ConflictingTypes String String
  deriving (Eq, Show)
