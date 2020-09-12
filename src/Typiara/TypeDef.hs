{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Typiara.TypeDef
  ( LinkId
  , TypeDef(..)
  , intoTypeTree
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

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
import Typiara.Utils (enumerateTree, mapLeft)

-- Definition of a type.
-- 
-- Provides a relatively stable, user facing interface.
-- Should not change as often as the implementation, hopefully.
type LinkId = String

data TypeDef constraint =
  TypeDef
    { shape :: Tree LinkId
    , constraints :: Map LinkId constraint
    }
  deriving (Eq, Show)

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
