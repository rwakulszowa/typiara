module Typiara.Apply
  ( apply
  , Applied(..)
  , ApplyErr(..)
  ) where

import Data.Function (on)

import Data.Tree (Tree(..))

import qualified Typiara.Constraint as Constraint
import qualified Typiara.TypeTree as TypeTree

import Typiara.TypeTree (TypeTree(..))
import Typiara.Utils (mapLeft)

import Typiara.Constraint (Constraint)

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
--    Whether that's acceptable is up to `Constraint`.
-- The result of a successful function application on a `TypeTree`.
-- Pieces are disjoint - they are not linked in any way.
data Applied c =
  Applied
    { arg :: TypeTree c
    , ret :: TypeTree c
    }
  deriving (Eq, Show)

data ApplyErr c
  = ShiftErr TypeTree.ShiftErr
  | MergeErr (TypeTree.MergeErr c)
  deriving (Eq, Show)

-- TODO: check if fun is applicable (add a Constraint method)
apply ::
     (Constraint c, Ord c, Show c)
  => TypeTree c
  -> TypeTree c
  -> Either (ApplyErr c) (Applied c)
apply fun arg = do
  merged <- merge' fun [0] arg
  argBranch <- merged `shift'` [0]
  retBranch <- merged `shift'` [1]
  return $ Applied {arg = argBranch, ret = retBranch}
  where
    merge' x path y = mapLeft MergeErr $ TypeTree.merge x path y
    shift' tree path = mapLeft ShiftErr $ TypeTree.shift tree path
