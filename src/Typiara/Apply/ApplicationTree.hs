module Typiara.Apply.ApplicationTree
  ( ApplicationTree(..)
  ) where

import Data.Map.Strict as Map
import Typiara.TypeTree (TypeTree, singleton)

-- | A tree representing curried function application in a type agnostic way.
-- The structure contains only the shape and node labels. If multiple nodes
-- are associated with the same label, they are considered linked and should
-- satisfy the same constraints.
--
-- An application boils down to binding a single function with a single
-- argument.
data ApplicationTree a
  -- | Leaf.
  = Unapplied a
  -- | Node. In a way, represents a set of parentheses, i.e.
  -- `(f (g x))` would be represented by 2 `Application` nodes and 3 `Unapplied`.
  | Application (ApplicationTree a) (ApplicationTree a)
  deriving (Eq, Show)

instance Functor ApplicationTree where
  fmap f (Unapplied x) = Unapplied $ f x
  fmap f (Application l r) = Application (f <$> l) (f <$> r)

instance Foldable ApplicationTree where
  foldMap f (Unapplied x) = f x
  foldMap f (Application l r) = foldMap f l `mappend` foldMap f r

instance Traversable ApplicationTree where
  traverse f (Unapplied x) = Unapplied <$> f x
  traverse f (Application l r) = Application <$> traverse f l <*> traverse f r
