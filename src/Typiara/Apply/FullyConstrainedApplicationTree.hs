module Typiara.Apply.FullyConstrainedApplicationTree
  ( FullyConstrainedApplicationTree
  , reduceConstraints
  , ReduceConstraintsError(..)
  ) where

import Data.Bifunctor (first)
import Typiara.ApplicableConstraint (ApplicableConstraint)
import Typiara.Apply (minimalApplicationTree)
import Typiara.Apply.ApplicationTree (ApplicationTree(..))
import Typiara.TypeTree (MergeErr, TypeTree, merge, mergeAt, shift)

type FullyConstrainedApplicationTree c = ApplicationTree (TypeTree c)

data ReduceConstraintsError c
  = AddingFunConstraintError (MergeErr c)
  | MergeApplyError (MergeErr c)
  | ShiftError
  deriving (Eq, Show)

-- | If all constraints are known, we can replace an ApplicationTree with
-- the application result, i.e.
-- `(Int -> Int) apply (Int)` can be replaced with `Int`.
-- `Left` if constraints are incompatible.
reduceConstraints ::
     (Ord c, ApplicableConstraint c, Show c)
  => FullyConstrainedApplicationTree c
  -> Either (ReduceConstraintsError c) (TypeTree c)
reduceConstraints (Unapplied t) = Right t
reduceConstraints (Application l r) = do
  l <- reduceConstraints l
  r <- reduceConstraints r
  -- ^ Reduce children.
  merged <- first MergeApplyError $ l `mergeApply` r
  -- ^ Merge two type trees. This bit validates that constraints are compatible.
  returnType merged
  -- ^ We no longer need fun and arg types - they've been provided from the
  -- outside, so we can just drop them and replace the tree with the result.
  where
    mergeApply fun arg = mergeAt fun [0] arg
    returnType tt = first (const ShiftError) $ tt `shift` [1]
