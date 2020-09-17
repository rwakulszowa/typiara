{-# LANGUAGE ScopedTypeVariables #-}

module Typiara.TypeTree
  ( TypeTree(..)
  , singleton
  , Constraint
  , merge
  , mergeAt
  , MergeErr(..)
  , shift
  , ShiftErr(..)
  , asTree
  ) where

import Data.Function (on)

import Data.Tree (Tree(..))

import qualified Typiara.Constraint as Constraint
import qualified Typiara.LinkedTree as LinkedTree

import Typiara.LinkedTree (LinkedTree(..))
import Typiara.Path (Path(..))
import Typiara.Utils (mapLeft, maybeError)

import Typiara.Constraint (Constraint, ConstraintErr(..))

-- A full definition of a type.
newtype TypeTree c =
  TypeTree
    { impl :: LinkedTree c
    }
  deriving (Show, Eq)

singleton = TypeTree . LinkedTree.singleton

data MergeErr c
  = ShapeErr (LinkedTree.MergeErr c)
  | ConflictingConstraints (ConstraintErr c)
  deriving (Eq, Show)

mergeAt ::
     (Ord c, Constraint c, Show c)
  => TypeTree c
  -> Path
  -> TypeTree c
  -> Either (MergeErr c) (TypeTree c)
mergeAt (TypeTree leftImpl) path (TypeTree rightImpl) = do
  mergedImpl <- mapLeft ShapeErr $ LinkedTree.merge leftImpl path rightImpl
  withReducedConstraints <-
    mapLeft ConflictingConstraints $
    traverse Constraint.mergeConstraints mergedImpl
  return $ TypeTree withReducedConstraints

merge x = mergeAt x []

data ShiftErr =
  PathNotFound
  deriving (Eq, Show)

shift :: (Constraint c) => TypeTree c -> Path -> Either ShiftErr (TypeTree c)
shift (TypeTree impl) path =
  TypeTree <$> maybeError PathNotFound (path `LinkedTree.shift` impl)

asTree :: TypeTree c -> LinkedTree c
asTree (TypeTree impl) = impl
