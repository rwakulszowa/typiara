{-# LANGUAGE ScopedTypeVariables #-}

module Typiara.TypeTree
  ( TypeTree(..)
  , Constraint
  , merge
  , MergeErr(..)
  , shift
  , ShiftErr(..)
  , apply
  , ApplyErr(..)
  , asTree
  ) where

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

data MergeErr c
  = ShapeErr (LinkedTree.MergeErr c)
  | ConflictingConstraints (ConstraintErr c)
  deriving (Eq, Show)

merge ::
     (Ord c, Constraint c, Show c)
  => TypeTree c
  -> Path
  -> TypeTree c
  -> Either (MergeErr c) (TypeTree c)
merge (TypeTree leftImpl) path (TypeTree rightImpl) = do
  mergedImpl <- mapLeft ShapeErr $ LinkedTree.merge leftImpl path rightImpl
  withReducedConstraints <-
    mapLeft ConflictingConstraints $
    traverse Constraint.mergeConstraints mergedImpl
  return $ TypeTree withReducedConstraints

data ShiftErr =
  PathNotFound
  deriving (Eq, Show)

shift :: (Constraint c) => TypeTree c -> Path -> Either ShiftErr (TypeTree c)
shift (TypeTree impl) path =
  TypeTree <$> maybeError PathNotFound (path `LinkedTree.shift` impl)

data ApplyErr c
  = ShiftErr ShiftErr
  | MergeErr (MergeErr c)
  deriving (Eq, Show)

-- TODO: check if fun is applicable (add a Constraint method)
apply fun arg = merge' fun [0] arg >>= shift' [1]
  where
    merge' x path y = mapLeft MergeErr $ merge x path y
    shift' path tree = mapLeft ShiftErr $ shift tree path

asTree :: TypeTree c -> LinkedTree c
asTree (TypeTree impl) = impl
