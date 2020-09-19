{-# LANGUAGE OverloadedLists #-}

module ApplySpec
  ( spec
  ) where

import Typiara.Apply

import Test.Hspec

import qualified Data.Map.Strict as Map

import Control.Monad ((>>=))
import Data.Char (toUpper)
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

buildLookup :: (Ord k) => [(k, v)] -> (k -> Maybe v)
buildLookup assocs k = Map.fromList assocs Map.!? k

-- Helper for building function types.
-- Quite limited. All ids of the same value are considered linked.
tx ids =
  let (shape, constraints) = build 0 ids
   in fromRight . TypeDef.intoTypeTree $
      TypeDef shape (Map.fromList constraints)
  where
    build _ [x] = (Node x [], [(x, ConstraintId $ toUpper <$> x)])
    build i (x:xs) =
      let fId = "f" ++ show i
          (childTree, childConstraints) = build (i + 1) xs
       in ( Node fId [Node x [], childTree]
          , [(x, ConstraintId $ toUpper <$> x), (fId, FunConstraint)] ++
            childConstraints)

spec :: Spec
spec =
  describe "apply" $ do
    let tAny = TypeTree.singleton AnyConstraint
    let tA = tx ["a"]
    let tB = tx ["b"]
    let tC = tx ["c"]
    describe "apply" $ do
      it "(a -> b) a" $ do
        let tAB = tx ["a", "b"]
        tAB `apply` tA `shouldBe` (Right $ Applied tA tB)
      it "hof: any a" $ do
        let tAnyAny = TypeTree.triple FunConstraint AnyConstraint AnyConstraint
        tAny `apply` tA `shouldBe` (Right $ Applied tA tAny)
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
                   [ ("f", FunConstraint)
                   , ("f'", FunConstraint)
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
        (arg `TypeTree.merge` arg0 >>= TypeTree.merge arg1) `shouldBe`
          (Left . ConflictingConstraints . ConstraintErr $ ConstraintId "A")
    describe "applyWithContext" $ do
      it "(a -> b) x" $ do
        let tAB = tx ["a", "b"]
        tAB `applyWithContext`
          (ApplicationContext
             {argTypeLookup = buildLookup [('a', tAny)], argIds = ['a']}) `shouldBe`
          Right (tB, [('a', tA)])
      it "(a -> a -> b) a a" $
        tx ["a", "a", "b"] `applyWithContext`
        (ApplicationContext
           { argTypeLookup = buildLookup [('a', tA), ('a', tA)]
           , argIds = ['a', 'a']
           }) `shouldBe`
        Right (tB, [('a', tA)])
      it "(a -> b -> c) a b" $
        tx ["a", "b", "c"] `applyWithContext`
        (ApplicationContext
           { argTypeLookup = buildLookup [('a', tA), ('b', tB)]
           , argIds = ['a', 'b']
           }) `shouldBe`
        Right (tC, [('a', tA), ('b', tB)])
      it "(a -> b -> b) x x" $
        tx ["a", "b", "b"] `applyWithContext`
        (ApplicationContext
           {argTypeLookup = buildLookup [('x', tAny)], argIds = ['x', 'x']}) `shouldBe`
        (Left $
         ConflictingArgError
           'x'
           (ConflictingConstraints . ConstraintErr $ ConstraintId "B"))
      it "(a -> b -> c) x x" $
        tx ["a", "b", "c"] `applyWithContext`
        (ApplicationContext
           {argTypeLookup = buildLookup [('a', tAny)], argIds = ['a', 'a']}) `shouldBe`
        (Left $
         ConflictingArgError
           'a'
           (ConflictingConstraints . ConstraintErr $ ConstraintId "B"))
