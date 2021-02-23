{-# LANGUAGE DeriveTraversable #-}

module Typiara.SampleTyp
  ( SampleTyp(..)
  ) where

import Typiara.FT (FT(..))
import Typiara.Typ (Typ(..), UnifyError(..), UnifyResult(..))

-- | Sample implementation.
data SampleTyp a
  = Seq a
  | Bool
  | Num
  | Str
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)

instance Typ SampleTyp where
  unify Nil a = Right (Unified a)
  unify a Nil = unify Nil a
  unify (T (Seq a)) (T (Seq b)) = Right (TypeVarsToUnify [(a, b)])
  unify (F a b) (F a' b') = Right (TypeVarsToUnify [(a, a'), (b, b')])
  unify x y =
    if x == y
      then Right (Unified x)
      else Left (ConflictingTypes x y)
