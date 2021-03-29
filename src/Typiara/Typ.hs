module Typiara.Typ
  ( Typ(..)
  , UnifyResult(..)
  , FTUnifyResult(..)
  , UnifyError(..)
  , unifyFT
  ) where

import           Data.Data           (Data)
import           Typiara.Data.Tagged
import           Typiara.FT

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

-- | Unify two FT types.
-- Wrap user defined `unify` implementation with a handler for core types defined in `FT`.
unifyFT ::
     (Typ t, Tagged t v, Eq (t v), Eq v, Data v)
  => FT t v
  -> FT t v
  -> Either UnifyError (FTUnifyResult t v)
-- FT types.
-- Nils unify with anything.
unifyFT Nil a = Right (FTUnifyResult a [])
unifyFT a Nil = unifyFT Nil a
-- There is a link on either side. All variables are unified to the same ident, the result is linked.
unifyFT (F a b) (F a' b')
  | a == b || a' == b' =
    Right (FTUnifyResult (F a a) [(a, b), (a, a'), (a, b')])
-- No links. Propagate pairwise, but do not introduce any links.
unifyFT (F a b) (F a' b') = Right (FTUnifyResult (F a b) [(a, a'), (b, b')])
-- Typ t
unifyFT (T a) (T b) = wrapResult <$> unify a b
-- Catchall.
-- to się zawsze wywali, bo x zawsze != y (FT się nie zgadzają).
-- TODO: wynieść do góry, ew. podać jako helper? I tak tutaj nic nie wejdzie, bo (T a) złapie wszystko
unifyFT x y =
  if x == y
    then Right (FTUnifyResult x [])
    else Left (ConflictingTypes (tag x) (tag y))

unifyEq x y =
  if x == y
    then Right (FTUnifyResult x [])
    else Left (FTConflictingTypes (tag x) (tag y))

-- | Unification may produce side effects, e.g. when two complex types are
-- unified and two different type variables appear at the same index, they have
-- to be merged into one.
data UnifyResult t v =
  UnifyResult
    { unified         :: t v
    , typeVarsToUnify :: [(v, v)]
    }
  deriving (Eq, Show, Ord)

-- | FT compatible wrapper for `UnifyResult`.
data FTUnifyResult t v =
  FTUnifyResult
    { ftUnified         :: FT t v
    , ftTypeVarsToUnify :: [(v, v)]
    }
  deriving (Eq, Show, Ord)

wrapResult (UnifyResult u t) =
  FTUnifyResult {ftUnified = T u, ftTypeVarsToUnify = t}

data UnifyError
  = ConflictingTypes String String
  | FTConflictingTypes String String
  deriving (Eq, Show)
