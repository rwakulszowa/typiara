module Typiara.Apply
  ( apply
  , Applied
  , applied
  , appliedTree
  , appliedArgN
  , appliedRet
  , ApplyErr(..)
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
