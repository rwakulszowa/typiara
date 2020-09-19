module Typiara.Apply
  ( apply
  , Applied(..)
  , ApplyErr(..)
  , applyWithContext
  , ApplyWithContextErr(..)
  , ApplicationContext(..)
  ) where

import qualified Data.Map.Strict as Map

import Control.Monad (foldM)
import Data.Function (on)
import Data.Map (Map)
import Data.Tree (Tree(..))

import qualified Typiara.Constraint as Constraint
import qualified Typiara.TypeTree as TypeTree

import Typiara.TypeTree (MergeErr, TypeTree(..))
import Typiara.Utils (fromRight, mapLeft)

import Typiara.ApplicableConstraint
  ( ApplicableConstraint
  , funConstraint
  , nilConstraint -- TODO: consider moving to `Constraint`
  )

-- Application of `TypeTree`s.
--
-- Application boils down to:
--  - merging an argument tree into a function tree, inferring new constraints in the process
--  - decomposing the result into an argument tree and a result tree
--
-- To validate that a function can be applied with a given set of arguments, the caller should
-- add the inferred `arg` requirements with the application context, e.g.
--  - given a function `Fun :: Num -> Str -> Num`
--  - given an application `Fun x x`
--  - the caller is responsible for merging `arg` items of both `apply` calls, as they map to
--    the same reference
--  - the merged constraint will require `x` to satisfy Num and Str.
--    Whether that's acceptable is up to `ApplicableConstraint`.
-- The result of a successful function application on a `TypeTree`.
-- Pieces are disjoint - they are not linked in any way.
data Applied c =
  Applied
    { argType :: TypeTree c
    , retType :: TypeTree c
    }
  deriving (Eq, Show)

data ApplyErr c
  = ShiftErr TypeTree.ShiftErr
  | MergeErr (TypeTree.MergeErr c)
  | AddFunConstraintErr (TypeTree.MergeErr c)
  deriving (Eq, Show)

apply ::
     (ApplicableConstraint c, Ord c, Show c)
  => TypeTree c
  -> TypeTree c
  -> Either (ApplyErr c) (Applied c)
apply fun arg = do
  extendedFun <- addFunConstraint fun
  merged <- merge' extendedFun [0] arg
  argBranch <- merged `shift'` [0]
  retBranch <- merged `shift'` [1]
  return $ Applied {argType = argBranch, retType = retBranch}
    -- Merge with a minimal function application tree, extending its shape and constraints, if need be.
  where
    addFunConstraint funNode =
      mapLeft AddFunConstraintErr $
      TypeTree.merge funNode minimalApplicationTree
        -- Minimal tree representing an function.
        -- Branches are not linked. After merging with the main tree, its links will propagate.
      where
        minimalApplicationTree =
          TypeTree.triple funConstraint nilConstraint nilConstraint
    merge' x path = mapLeft MergeErr . TypeTree.mergeAt x path
    shift' tree path = mapLeft ShiftErr $ TypeTree.shift tree path

data ApplicationContext argId c =
  ApplicationContext
    { argTypeLookup :: argId -> Maybe (TypeTree c)
    , argIds :: [argId]
    }

data ApplyWithContextErr argId c
  = ApplyError (ApplyErr c)
  | ArgTypeLookupError argId
  | ConflictingArgError
      { conflictingArgId :: argId
      , err :: MergeErr c
      }
  deriving (Eq, Show)

-- Apply each arg to `fun`, from left to right.
-- Returns (returnType, argConstraints).
-- The size of argConstraints may be smaller than that of `args` if the same argument is applied multiple times.
-- If that's the case, individual constraints will be merged.
applyWithContext ::
     (Eq argId, Ord argId, ApplicableConstraint c, Ord c, Show c)
  => TypeTree c
  -> ApplicationContext argId c
  -> Either (ApplyWithContextErr argId c) (TypeTree c, Map argId (TypeTree c))
applyWithContext fun (ApplicationContext argTypeLookup argIds) = do
  (constraints, ret) <- applyEach argTypeLookup fun argIds
  mergedConstraintsPerId <-
    sequence . Map.mapWithKey mergeConstraints . group $ constraints
  return (ret, mergedConstraintsPerId)
  where
    group = Map.fromListWith mappend . fmap (\(k, v) -> (k, [v]))
    mergeConstraints argId =
      mapLeft (ConflictingArgError argId) . foldM' TypeTree.merge
      where
        foldM' f (x:xs) = foldM f x xs

-- Traverse the `fun` tree, applying args sequentially.
--
-- Checks that function can be applied with given args, but doesn't check whether
-- a given combination of args wouldn't cause a conflict.
--
-- Each application is independent - if the same argId is passed in multiple times,
-- it's invdividual constraints are not combined.
applyEach argTypeLookup fun = foldM (applyOne argTypeLookup) ([], fun)
  where
    applyOne argTypeLookup (constraintsAcc, fun) argId = do
      argType <-
        maybe (Left $ ArgTypeLookupError argId) Right $ argTypeLookup argId
      (Applied inferredArgType retType) <-
        mapLeft ApplyError $ fun `apply` argType
      return ((argId, inferredArgType) : constraintsAcc, retType)
