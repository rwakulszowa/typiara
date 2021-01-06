module Typiara.Apply.PartiallyConstrainedApplicationTree
  ( PartiallyConstrainedApplicationTree
  , injectConstraints
  , reduceConstrainedNodes
  ) where

import Typiara.ApplicableConstraint
import Typiara.Apply.ApplicationTree
  ( ApplicationTree(..)
  , traverseWithReplacement
  )
import Typiara.Apply.FullyConstrainedApplicationTree
  ( FullyConstrainedApplicationTree
  , ReduceConstraintsError(..)
  , reduceConstraints
  )
import Typiara.TypeTree (TypeTree)

type PartiallyConstrainedApplicationTree a c
   = ApplicationTree (Either a (TypeTree c))

-- | Iterate over each node, looking ids up in a provided function.
-- Values not found in the function are represented as `Left` nodes.
injectConstraints ::
     (a -> Maybe (TypeTree c))
  -> ApplicationTree a
  -> PartiallyConstrainedApplicationTree a c
injectConstraints lookup = fmap (\a -> maybe (Left a) Right (lookup a))

-- | Fully constrained trees can be reduced to their application result.
intoFullyConstrained ::
     PartiallyConstrainedApplicationTree a c
  -> Maybe (FullyConstrainedApplicationTree c)
intoFullyConstrained = either (const Nothing) Just . sequence

-- | Try to convert a tree into its return value.
-- `Nothing` if the tree is not fully constrained, and cannot be reduced.
-- `Just Right` if reuction succeeded.
-- `Just Left` if the tree has conflicting requirements.
tryReduce ::
     (Show c, Ord c, ApplicableConstraint c)
  => PartiallyConstrainedApplicationTree a c
  -> Maybe (Either (ReduceConstraintsError c) (TypeTree c))
tryReduce = fmap reduceConstraints . intoFullyConstrained

-- | Reduce nodes that can be reduced, potentially returning merge errors.
-- Non-constrained branches are left intact.
reduceConstrainedNodes ::
     (Show c, ApplicableConstraint c, Ord c)
  => PartiallyConstrainedApplicationTree a c
  -> Either (ReduceConstraintsError c) (PartiallyConstrainedApplicationTree a c)
reduceConstrainedNodes = traverseWithReplacement reduceOne
  where
    reduceOne t =
      case (tryReduce t) of
        Nothing -> Right t
        -- ^ `Nothing`s are not errors. Return the original tree, untouched.
        (Just result) -> (Unapplied . Right) <$> result
