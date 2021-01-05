module Typiara.Apply
  ( apply
  , Applied
  , applied
  , appliedTree
  , appliedArgN
  , appliedRet
  , ApplyErr(..)
  , applyWithContext
  , AppliedWithContext(..)
  , ApplyWithContextErr(..)
  , ApplicationContext(..)
  , minimalApplicationTree
  ) where

import qualified Data.Map.Strict as Map

import Control.Monad (foldM)
import Data.Function (on)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Tree (Tree(..))
import Data.Tuple (swap)

import qualified Typiara.Constraint as Constraint
import qualified Typiara.TypeTree as TypeTree

import Typiara.Path (Path)
import Typiara.TypeTree (MergeErr, TypeTree(..))
import Typiara.Utils (fromRight, mapLeft, sequenceSnd, tenumerate)

import Typiara.ApplicableConstraint
  ( ApplicableConstraint
  , funConstraint
  , nilConstraint -- TODO: consider moving to `Constraint`
  )
import Typiara.Constraint (Constraint)

-- Application of `TypeTree`s.
--
-- Application boils down to merging an argument tree into a function tree, inferring new constraints in the process.
--
-- To validate that a function can be applied with a given set of arguments, the caller should
-- add the inferred `arg` requirements with the application context, e.g.
--  - given a function `Fun :: Num -> Str -> Num`
--  - given an application `Fun x x`
--  - the caller is responsible for merging `arg` items of both `apply` calls, as they map to
--    the same reference
--  - the merged constraint will require `x` to satisfy Num and Str.
--    Whether that's acceptable is up to `ApplicableConstraint`.
--
--
-- The result of a successful function application on a `TypeTree`.
-- Stores current application state, i.e. how many arguments have already been applied.
-- Each subsequent application will bind to the next argument.
data Applied c =
  Applied
    { appliedTree :: TypeTree c
    , appliedCount :: Int
    }
  deriving (Eq, Show)

applied :: TypeTree c -> Applied c
applied t = Applied t 0

appliedRet :: (Constraint c) => Applied c -> TypeTree c
appliedRet (Applied tt n) = fromJust $ vRoot n tt

-- `Nothing` if hasn't been applied `n` times.
-- It's guaranteed to find the item otherwise. If it doesn't (causing a crash), it's an error of failed preconditions.
appliedArgN :: (Constraint c) => Applied c -> Int -> Maybe (TypeTree c)
appliedArgN (Applied tt appliedCount) n =
  if appliedCount > n
    then Just $ fromJust $ argNode n tt
    else Nothing

-- Convenience lookup helpers.
ttAtPath :: (Constraint c) => TypeTree c -> Path -> Maybe (TypeTree c)
ttAtPath tt p = either (const Nothing) Just $ tt `TypeTree.shift` p

-- After each application, the virtual root moves to the return value of the previous root.
-- Trackes the current status of a partially applied curried function.
vRootPath :: Int -> Path
vRootPath n = replicate n 1

argPath n = vRootPath n ++ [0]

vRoot n = (`ttAtPath` vRootPath n)

argNode n = (`ttAtPath` argPath n)

data ApplyErr c
  = ShiftErr TypeTree.ShiftErr
  | MergeErr (TypeTree.MergeErr c)
  | AddFunConstraintErr (TypeTree.MergeErr c)
  deriving (Eq, Show)

apply ::
     (ApplicableConstraint c, Ord c, Show c)
  => TypeTree c
  -> Applied c
  -> Either (ApplyErr c) (Applied c)
apply arg (Applied tt appliedCount) =
  Applied <$> (addFunConstraint' tt appliedCount >>= merge' arg appliedCount) <*>
  pure (appliedCount + 1)
  where
    addFunConstraint' tt n = addFunConstraint tt (vRootPath n)
    merge' arg n fun = mapLeft MergeErr $ TypeTree.mergeAt fun (argPath n) arg

-- Merge with a minimal function application tree, extending its shape and constraints, if need be.
addFunConstraint tt path =
  mapLeft AddFunConstraintErr $ TypeTree.mergeAt tt path minimalApplicationTree

-- Minimal tree representing an function.
-- Branches are not linked. After merging with the main tree, its links will propagate.
-- Merging with a tree had already been marked a function will not modify it.
minimalApplicationTree :: (ApplicableConstraint c) => TypeTree c
minimalApplicationTree = TypeTree.triple funConstraint nilConstraint nilConstraint

data ApplicationContext argId c =
  ApplicationContext
    { argTypeLookup :: argId -> Maybe (TypeTree c)
    , argIds :: [argId]
    }

data AppliedWithContext argId c =
  AppliedWithContext
    { funType :: TypeTree c
    , retType :: TypeTree c
    , argTypes :: Map argId (TypeTree c)
    }
  deriving (Eq, Show)

data ApplyWithContextErr argId c
  = ApplyError (ApplyErr c)
  | ArgTypeLookupError argId
  | ConflictingArgError
      { conflictingArgId :: argId
      , err :: MergeErr c
      }
  | FailedLinkError argId [TypeTree c] -- Returned if shallow linking fails and linked items are not identical.
  deriving (Eq, Show)

-- Apply each arg to `fun`, from left to right.
-- Returns (functionType, Map argId argType).
-- If the same argId is applied multiple times, its nodes are merged together.
--
-- Decomposes the tree. Any links across trees are broken.
--
-- TODO: either:
--  - do not decompose the tree, to keep the links intact
--  - allow one to recompose the tree by exporting a proper linking mechanism
--  - or some combination of the above; introduce a mechanism of building trees of `Applied`, allowing composition and decomposition, instead of just a single function.
applyWithContext ::
     (Eq argId, Ord argId, ApplicableConstraint c, Ord c, Show c)
  => TypeTree c
  -> ApplicationContext argId c
  -> Either (ApplyWithContextErr argId c) (AppliedWithContext argId c)
applyWithContext fun (ApplicationContext argTypeLookup argIds) = do
  retBeforeLinking <- applyEach argTypeLookup fun argIds
  let idZipIndices = Map.assocs $ indicesPerId argIds
  -- For each arg that appears more than once in the arg list, shallow-link its nodes.
  appliedAndLinked <- foldM linkSingleArgIndices retBeforeLinking idZipIndices
  -- Look up final types for each arg index.
  idZipTypes <-
    mapM
      sequenceSnd
      [ (id, mapM (lookupIndexOrErr appliedAndLinked id) indices)
      | (id, indices) <- idZipIndices
      ]
  -- Validate that supposedly linked nodes are identical.
  deduplicatedIdToType <-
    mapM
      sequenceSnd
      [ ( argId
        , either (Left . FailedLinkError argId) Right (deduplicateOrErr types))
      | (argId, types) <- idZipTypes
      ]
  return
    (AppliedWithContext
       { funType = appliedTree appliedAndLinked
       , retType = appliedRet appliedAndLinked
       , argTypes = Map.fromList deduplicatedIdToType
       })
  where
    indicesPerId :: (Ord a) => [a] -> Map a [Int]
    indicesPerId = group . map swap . tenumerate
      where
        group = Map.fromListWith mappend . fmap (\(k, v) -> (k, [v]))
    lookupIndexOrErr applied argId index =
      maybe
        (Left $ ArgTypeLookupError argId)
        Right
        (applied `appliedArgN` index)
    deduplicateOrErr xs =
      case nub xs of
        [x] -> Right x
        xs' -> Left xs'
    linkSingleArgIndices applied (argId, is) =
      mapLeft (ConflictingArgError argId) $ linkIndices applied is
    linkIndices applied [i] = Right applied -- noop for a singleton index
    linkIndices (Applied tree count) (i:is) =
      Applied <$> foldM (linkIndices' i) tree is <*> pure count
      where
        linkIndices' i tree j = shallowLink tree (argPath i) (argPath j)
      -- Make two nodes equivalent by performing a two way merge (i.e. `merge tree a b`, `merge tree b a`),
      -- but not introducing a link, as defined by `LinkedTree`.
      -- Both nodes should be equivalent right after the operation, but any subsequent changes
      -- may not propagate to shallowly linked nodes.
      -- Should suffice for `apply` purposes, as the result is expected to be decomposed.
      --
      -- Should a linking mechanism be implemented in `TypeTree`, consider
      -- using it instead of this implementation.
        shallowLink tree pathA pathB =
          selfMerge tree pathA pathB >>= (\t -> selfMerge t pathB pathA)
          where
            selfMerge tree srcPath dstPath =
              TypeTree.mergeAt
                tree
                dstPath
                (fromRight $ tree `TypeTree.shift` srcPath) -- shift errors are not expected in this context.
                                                             -- Handle them properly should this become a standalone function.

-- Traverse the `fun` tree, applying args sequentially.
--
-- Checks that function can be applied with given args, but doesn't check whether
-- a given combination of args wouldn't cause a conflict.
--
-- Each application is independent - if the same argId is passed in multiple times,
-- it's invdividual constraints are not combined.
applyEach argTypeLookup fun = foldM (applyOne argTypeLookup) (applied fun)
  where
    applyOne argTypeLookup fun argId = do
      argType <-
        maybe (Left $ ArgTypeLookupError argId) Right $ argTypeLookup argId
      mapLeft ApplyError $ argType `apply` fun
