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
  unify Nil a = Right (UnifyResult a [])
  unify a Nil = unify Nil a
  unify (T (Seq a)) (T (Seq b)) = Right (UnifyResult (T (Seq a)) [(a, b)])
  unify (F a b) (F a' b')
    | a == b || a' == b' =
      Right (UnifyResult (F a a) [(a, b), (a, a'), (a, b')])
  -- ^ There is a link on either side. All variables are unified to the same ident, the result is linked.
  unify (F a b) (F a' b') = Right (UnifyResult (F a b) [(a, a'), (b, b')])
  -- ^ No links. Propagate pairwise, but do not introduce any links.
  unify x y =
    if x == y
      then Right (UnifyResult x [])
      else Left (ConflictingTypes (tag x) (tag y))

instance (Data a) => Tagged SampleTyp a where
  tag = show . toConstr
  -- TODO: try to reuse the magic `gunfold` function from `Data.Data`.
  fromTag "Bool" [] = Just Bool
  fromTag "Num" [] = Just Num
  fromTag "Str" [] = Just Str
  fromTag "Seq" [a] = Just (Seq a)
  fromTag _ _ = Nothing
