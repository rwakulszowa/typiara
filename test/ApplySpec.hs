{-# LANGUAGE OverloadedLists #-}

module ApplySpec
  ( spec
  ) where

import Typiara.Apply

import Test.Hspec

import qualified Data.Map.Strict as Map

import Control.Monad ((>>=))
import Data.Map (Map(..))
import Data.Tree (Tree(..))

import qualified Typiara.LinkedTree as LinkedTree
import qualified Typiara.TypeDef as TypeDef
import qualified Typiara.TypeTree as TypeTree

import TestConstraint (TestConstraint(..))
import Typiara.Constraint (ConstraintErr(..))
import Typiara.Link (Link(..))
import Typiara.LinkedTree (LinkedTree(..))
import Typiara.TypeDef (TypeDef(..))
import Typiara.TypeTree (MergeErr(..), TypeTree(..))
import Typiara.Utils (fromRight)

linkedTree' :: Tree Link -> Map Link a -> LinkedTree a
linkedTree' s v = fromRight $ LinkedTree.linkedTree s v

mergeRoot x = TypeTree.merge x []

spec :: Spec
spec =
  describe "apply" $ do
    it "(a -> b) a" $ do
      let fun =
            fromRight . TypeDef.intoTypeTree $
            TypeDef
              (Node "f" [Node "a" [], Node "b" []])
              (Map.fromList
                 [ ("f", ConstraintId "F")
                 , ("a", ConstraintId "A")
                 , ("b", ConstraintId "B")
                 ])
      let arg = TypeTree.singleton (ConstraintId "A")
      fun `apply` arg `shouldBe`
        (Right $
         Applied
           (TypeTree $
            linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")])
           (TypeTree $
            linkedTree' (Node (Link "b") []) [(Link "b", ConstraintId "B")]))
    describe "validate application" $
      it "Fun :: A -> B -> A; Fun x x" $
    -- Apply the same argument to two different constraints.
    -- Validate if it's possible to detect the issue.
       do
        let fun =
              fromRight . TypeDef.intoTypeTree $
              TypeDef
                (Node "f" [Node "a" [], Node "f'" [Node "b" [], Node "c" []]])
                (Map.fromList
                   [ ("f", ConstraintId "F")
                   , ("f'", ConstraintId "F")
                   , ("a", ConstraintId "A")
                   , ("b", ConstraintId "B")
                   , ("c", ConstraintId "B")
                   ])
    -- The argument itself has no initial constraints. Each individual application
    -- should succeed.
        let arg = TypeTree.singleton AnyConstraint
    -- Gather constraints after applying the function twice.
    -- `Fun x x`
        let (arg0, arg1) =
              fromRight $ do
                (Applied arg0 ret0) <- fun `apply` arg
                (Applied arg1 ret1) <- ret0 `apply` arg
                return (arg0, arg1)
    -- Merge the constraints, because the same reference (`x`) is used in both cases.
        (arg `mergeRoot` arg0 >>= mergeRoot arg1) `shouldBe`
          (Left . ConflictingConstraints . ConstraintErr $ ConstraintId "A")
