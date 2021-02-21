{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Typiara.TypeDef
  ( LinkId
  , TypeDef(..)
  , singleton
  , intoTypeTree
  , fromTypeTree
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Data.Functor.Classes (compare1)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tree (Tree)

import Data.List ((\\), nub)

import qualified Typiara.LinkedTree as LinkedTree
import qualified Typiara.TypeTree as TypeTree

import Typiara.Constraint (Constraint)
import Typiara.Link (Link(..))
import Typiara.LinkedTree (LinkedTree(..))
import Typiara.Path (Step)
import Typiara.TypeTree (TypeTree(..))
import Typiara.Utils (enumerateTree, mapKeysRejectConflicts, mapLeft)

-- Definition of a type.
-- 
-- Provides a relatively stable, user facing interface.
-- Should not change as often as the implementation, hopefully.
--
-- `TypeDef` is expected to be used in user facing scenarios. `TypeTree` is allowed
-- to grow aribitrarily complex, therefore serializing it and showing to the user
-- may not be a good idea. `TypeDef` solves this problem by providing a readable,
-- easily serializable representation of a type.
type LinkId = String

data TypeDef constraint =
  TypeDef
    { shape :: Tree LinkId
    , constraints :: Map LinkId constraint
    }
  deriving (Eq, Show, Ord)

-- To derive `Ord` on `TypeDef`.
instance Ord (Tree LinkId) where
  compare = compare1

singleton = fromTypeTree . TypeTree.singleton

intoLinkedTree ::
     (Show c, Eq c, Constraint c, Ord c)
  => TypeDef c
  -> Either String (LinkedTree c)
intoLinkedTree (TypeDef shape constraints) =
  let shapeOfLinks = Link <$> shape
      constraintsOfLinks = Map.mapKeys Link constraints
   in mapLeft show $ LinkedTree.linkedTree shapeOfLinks constraintsOfLinks

intoTypeTree ::
     (Constraint c, Show c, Ord c, Eq c)
  => TypeDef c
  -> Either String (TypeTree c)
intoTypeTree = fmap TypeTree . intoLinkedTree

fromTypeTree :: TypeTree c -> TypeDef c
fromTypeTree (TypeTree linkedTree) =
  let freshTree = LinkedTree.refreshLinks linkedTree
   in fromTypeTree' (LinkedTree.shape freshTree) (values freshTree)
  where
    fromTypeTree' shape values =
      TypeDef (unlink <$> shape) (mapKeys' unlink values)
      where
        mapKeys' f m = Maybe.fromJust $ mapKeysRejectConflicts f m -- Guaranteed by `LinkedTree` contract not to cause conflicts. Crash explicitly if not true.
        unlink (Link value) = value
