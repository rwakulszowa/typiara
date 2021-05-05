{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Typiara.FT
  ( FT(..)
  , unifyFT
  , FTUnifyResult(..)
  ) where

import           Data.Data           (Data)
import           Data.Hashable
import           GHC.Generics
import           Typiara.Data.Tagged (Tagged (..))
import           Typiara.TypDef      (TypDef (..), UnifyError (..),
                                      UnifyResult (..))

-- | Function or not-a-function (or an empty type).
-- Functions are special. All other types are provided by the user, but
-- functions require extra care.
--
-- `Nil` is used when no constraints are known.
data FT t v
  = F v v
  | T (t v)
  | Nil
  deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable, Generic)

instance (Tagged t) => Tagged (FT t) where
  tag (F _ _) = "F"
  tag (T t)   = "T." ++ tag t
  tag Nil     = "Nil"
  fromTag "Nil" []        = Just Nil
  fromTag "F" [a, b]      = Just (F a b)
  fromTag ('T':'.':ts) xs = T <$> fromTag ts xs
  fromTag _ _             = Nothing

instance (Hashable (t v), Hashable v) => Hashable (FT t v)

-- | Unify two FT types.
-- Wrap user defined `unify` implementation with a handler for core types defined in `FT`.
unifyFT ::
     (TypDef t, Tagged t, Eq (t Int))
  => FT t Int
  -> FT t Int
  -> Either UnifyError (FTUnifyResult t Int)
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
-- Matches only situation where the top level `FT` constructors are different.
unifyFT x y = Left (ConflictingTypes (tag x) (tag y))

-- | FT compatible wrapper for `UnifyResult`.
data FTUnifyResult t v =
  FTUnifyResult
    { ftUnified         :: FT t v
    , ftTypeVarsToUnify :: [(v, v)]
    }
  deriving (Eq, Show, Ord)

wrapResult (UnifyResult u t) =
  FTUnifyResult {ftUnified = T u, ftTypeVarsToUnify = t}
