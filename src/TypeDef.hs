{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module TypeDef
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

import qualified LinkedTree
import qualified TypeTree

import Constraint (Constraint)
import Link (Link(..))
import LinkedTree (LinkedTree(..))
import Path (Step)
import TypeTree (TypeTree(..))
import Utils (enumerateTree, mapLeft)

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
   in Right $ LinkedTree shapeOfLinks constraintsOfLinks

intoTypeTree ::
     (Constraint c, Show c, Ord c, Eq c)
  => TypeDef c
  -> Either String (TypeTree c)
intoTypeTree = fmap TypeTree . intoLinkedTree
