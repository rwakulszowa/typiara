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

import qualified Typiara.Dag as Dag
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
spec = do
  let apply' fun arg = arg `apply` applied fun
  let tAny = TypeTree.singleton AnyConstraint
  let tA = tx ["a"]
  let tB = tx ["b"]
  let tC = tx ["c"]
  describe "apply" $ do
    it "(a -> b) a" $ do
      let tAB = tx ["a", "b"]
      appliedTree <$> tAB `apply'` tA `shouldBe` Right tAB
    it "hof: any a" $
      appliedTree <$>
      tAny `apply'` tA `shouldBe`
      (Right $ TypeTree.triple FunConstraint (ConstraintId "A") AnyConstraint)
    it "incompatible pair introducing a cycle" $
      -- Validates that invalid arguments are rejected.
      -- The scenario reproduced below used to cause an infinite loop during DAG reconstruction
      -- due to `x[0]` and `y` link structure.
     do
      let x =
            fromRight . TypeDef.intoTypeTree $
            TypeDef
              (Node "a" [Node "b" [Node "d" [], Node "d" []], Node "c" []])
              [ ("a", FunConstraint)
              , ("b", FunConstraint)
              , ("c", AnyConstraint)
              , ("d", AnyConstraint)
              ]
      let y =
            fromRight . TypeDef.intoTypeTree $
            TypeDef
              (Node "x" [Node "y" [], Node "z" [Node "y" []]])
              [("x", FunConstraint), ("y", AnyConstraint), ("z", AnyConstraint)]
      x `apply'` y `shouldBe`
        (Left . MergeErr . ShapeErr . LinkedTree.DagMergeErr $ Dag.Cycle)
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
      let (Right result) = pure (applied fun) >>= apply arg >>= apply arg
      let (Just arg0) = appliedArgN result 0
      let (Just arg1) = appliedArgN result 1
    -- Merge the constraints, because the same reference (`x`) is used in both cases.
      (arg `TypeTree.merge` arg0 >>= TypeTree.merge arg1) `shouldBe`
        (Left . ConflictingConstraints . ConstraintErr $ ConstraintId "A")
  describe "applyWithContext" $ do
    it "(a -> b) x" $ do
      let tAB = tx ["a", "b"]
      tAB `applyWithContext`
        (ApplicationContext
           {argTypeLookup = buildLookup [('a', tAny)], argIds = ['a']}) `shouldBe`
        Right
          (AppliedWithContext
             {funType = tAB, retType = tB, argTypes = [('a', tA)]})
    it "(a -> a -> b) a a" $ do
      let tAAB = tx ["a", "a", "b"]
      tAAB `applyWithContext`
        (ApplicationContext
           { argTypeLookup = buildLookup [('a', tA), ('a', tA)]
           , argIds = ['a', 'a']
           }) `shouldBe`
        Right
          (AppliedWithContext
             {funType = tAAB, retType = tB, argTypes = [('a', tA)]})
    it "(a -> b -> c) a b" $ do
      let tABC = tx ["a", "b", "c"]
      tABC `applyWithContext`
        (ApplicationContext
           { argTypeLookup = buildLookup [('a', tA), ('b', tB)]
           , argIds = ['a', 'b']
           }) `shouldBe`
        Right
          (AppliedWithContext
             {funType = tABC, retType = tC, argTypes = [('a', tA), ('b', tB)]})
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
