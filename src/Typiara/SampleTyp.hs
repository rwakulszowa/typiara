{-# LANGUAGE DeriveTraversable, DeriveDataTypeable,
  MultiParamTypeClasses, FlexibleInstances #-}

module Typiara.SampleTyp
  ( SampleTyp(..)
  ) where

import Data.Data (Data, Typeable, toConstr)
import Typiara.Data.Tagged (Tagged(..))
import Typiara.FT (FT(..))
import Typiara.Typ (Typ(..), UnifyError(..), UnifyResult(..))

-- | Sample implementation.
data SampleTyp a
  = Seq a
  | Bool
  | Num
  | Str
  deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable, Data, Typeable)

instance Typ SampleTyp where
  unify Nil a = Right (Unified a)
  unify a Nil = unify Nil a
  unify (T (Seq a)) (T (Seq b)) = Right (TypeVarsToUnify [(a, b)])
  unify (F a b) (F a' b') = Right (TypeVarsToUnify [(a, a'), (b, b')])
  unify x y =
    if x == y
      then Right (Unified x)
      else Left (ConflictingTypes x y)

instance (Data a) => Tagged SampleTyp a where
  tag = show . toConstr
  -- TODO: try to reuse the magic `gunfold` function from `Data.Data`.
  fromTag "Bool" [] = Just Bool
  fromTag "Num" [] = Just Num
  fromTag "Str" [] = Just Str
  fromTag "Seq" [a] = Just (Seq a)
  fromTag _ _ = Nothing
