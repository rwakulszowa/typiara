{-# LANGUAGE FlexibleInstances #-}

module Typiara.Type
  ( Type(..)
  , RigidType(..)
  , Requirement(..)
  ) where

import qualified Data.Set as Set

import Data.Set (Set)
import Data.Tree (Tree(..))

import Data.Semigroup (sconcat)

import Typiara.Constraint

-- Simplified, statically defined, Haskell-like type system.
-- No inheritance. A type can be bound by up to 1 `RigidType` and multiple `Requirement`s.
-- `RigidType` must satisfy a requirement directly.
-- An actual type. One `RigidType` may exist per tree node.
data RigidType
  = Int
  | Char
  | Seq
  | Fun
  deriving (Eq, Show, Ord)

-- An additional requirement for a node.
-- Requirements accumulate as a side effect of function composition.
data Requirement
  = Show
  | Eq
  deriving (Eq, Show, Ord)

-- In this implementation, only `RigidType`s define if a `Requirement` is satisfied.
-- Derived requirements, e.g. `Ord a => Eq (T a)`, are not yet supported.
satisfies :: RigidType -> Requirement -> Bool
satisfies Int Show = True
satisfies Char Show = True
satisfies Int Eq = True
satisfies Char Eq = True
satisfies Seq _ = error "Derived requirements not yet implemented."
satisfies _ _ = False

data Type
  = RigidType RigidType
  | Requirement Requirement
  deriving (Eq, Show, Ord)

isRigid (RigidType _) = True
isRigid _ = False

partitionType :: [Type] -> ([RigidType], [Requirement])
partitionType = foldl appendToBucket ([], [])
  where
    appendToBucket (rigids, requirements) (RigidType r) =
      (r : rigids, requirements)
    appendToBucket (rigids, requirements) (Requirement r) =
      (rigids, r : requirements)

-- TODO: newtype Types; non trivial constructor
-- what used to be an invalid TypeTree has now become an invalid (Set Type) -> add validation on this layer
instance Constraint (Set Type) where
  mergeConstraints =
    fmap Set.fromList . mergeConstraints' . partitionType . Set.elems . sconcat
    where
      mergeConstraints' ::
           ([RigidType], [Requirement])
        -> Either (ConstraintErr (Set Type)) [Type]
      mergeConstraints' ([], reqs) = Right (Requirement <$> reqs)
      mergeConstraints' ([rigid], reqs) =
        case (not . satisfies rigid) `filter` reqs of
          [] -> Right (RigidType rigid : (Requirement <$> reqs))
          conflicts ->
            Left $
            ConstraintErr $
            Set.fromList (RigidType rigid : (Requirement <$> conflicts))
      mergeConstraints' (rigids, _) =
        Left $ ConstraintErr $ Set.fromList $ RigidType <$> rigids
