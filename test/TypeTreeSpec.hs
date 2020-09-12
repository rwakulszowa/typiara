{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeTreeSpec
  ( spec
  ) where

import Typiara.TypeTree

import Test.Hspec

import Data.Map (Map(..))
import Data.Tree (Tree(..))

import qualified Typiara.LinkedTree as LinkedTree
import qualified Typiara.TypeTree as TypeTree

import TestConstraint (TestConstraint(..))
import Typiara.Link (Link(..))
import Typiara.LinkedTree (LinkedTree(..))

unwrap (Right x) = x
unwrap (Left err) = error . show $ err

linkedTree' :: Tree Link -> Map Link a -> LinkedTree a
linkedTree' s v = unwrap $ LinkedTree.linkedTree s v

spec :: Spec
spec = do
  describe "constructors" $
    it "singleton" $
    singleton (ConstraintId "A") `shouldBe`
    (TypeTree $ linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")])
  describe "merge" $ do
    describe "nil offset" $ do
      it "singleton singleton" $ do
        let x =
              TypeTree $
              linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")]
        let y =
              TypeTree $
              linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")]
        merge x [] y `shouldBe` Right x
      it "subtree" $ do
        let x =
              TypeTree $
              linkedTree'
                (Node (Link "a") [Node (Link "b") []])
                [(Link "a", ConstraintId "A"), (Link "b", ConstraintId "B")]
        let y =
              TypeTree $
              linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")]
        merge x [] y `shouldBe` Right x
      it
        "wide linked left + tall right copies right's children across left's branches" $ do
        let x =
              TypeTree $
              linkedTree'
                (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
                [(Link "a", ConstraintId "A"), (Link "b", ConstraintId "B")]
        let y =
              TypeTree $
              linkedTree'
                (Node (Link "a") [Node (Link "b") [Node (Link "c") []]])
                [ (Link "a", ConstraintId "A")
                , (Link "b", ConstraintId "B")
                , (Link "c", ConstraintId "C")
                ]
        merge x [] y `shouldBe`
          (Right $
           TypeTree $
           linkedTree'
             (Node
                (Link "a")
                [ Node (Link "b") [Node (Link "c") []]
                , Node (Link "b") [Node (Link "c") []]
                ])
             [ (Link "a", ConstraintId "A")
             , (Link "b", ConstraintId "B")
             , (Link "c", ConstraintId "C")
             ])
    describe "[0] offset" $ do
      it "shifted exact match" $ do
        let x =
              TypeTree $
              linkedTree'
                (Node (Link "a") [Node (Link "b") []])
                [(Link "a", ConstraintId "A"), (Link "b", ConstraintId "B")]
        let y =
              TypeTree $
              linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "B")]
        merge x [0] y `shouldBe` Right x
      it "different shapes" $ do
        let x =
              TypeTree $
              linkedTree'
                (Node (Link "a") [Node (Link "b") [], Node (Link "c") []])
                [ (Link "a", ConstraintId "A")
                , (Link "b", ConstraintId "B")
                , (Link "c", ConstraintId "C")
                ]
        let y =
              TypeTree $
              linkedTree'
                (Node (Link "b") [Node (Link "d") []])
                [(Link "b", ConstraintId "B"), (Link "d", ConstraintId "D")]
        merge x [0] y `shouldBe`
          (Right $
           TypeTree $
           linkedTree'
             (Node
                (Link "a")
                [Node (Link "b") [Node (Link "d") []], Node (Link "c") []])
             [ (Link "a", ConstraintId "A")
             , (Link "b", ConstraintId "B")
             , (Link "c", ConstraintId "C")
             , (Link "d", ConstraintId "D")
             ])
      it "with links" $ do
        let x =
              TypeTree $
              linkedTree'
                (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
                [(Link "a", ConstraintId "A"), (Link "b", ConstraintId "B")]
        let y =
              TypeTree $
              linkedTree' (Node (Link "b") []) [(Link "b", ConstraintId "B")]
        merge x [0] y `shouldBe` Right x
  describe "shift" $
    it "simple path" $ do
      let tree =
            TypeTree $
            linkedTree'
              (Node (Link "a") [Node (Link "b") []])
              [(Link "a", ConstraintId "A"), (Link "b", ConstraintId "B")]
      tree `shift`
        [0] `shouldBe`
        (Right $
         TypeTree $
         linkedTree' (Node (Link "b") []) [(Link "b", ConstraintId "B")])
  describe "apply" $
    it "(a -> b) a" $ do
      let fun =
            TypeTree $
            linkedTree'
              (Node (Link "fun") [Node (Link "a") [], Node (Link "b") []])
              [ (Link "fun", ConstraintId "F")
              , (Link "a", ConstraintId "A")
              , (Link "b", ConstraintId "B")
              ]
      let arg =
            TypeTree $
            linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")]
      fun `apply` arg `shouldBe`
        (Right $
         TypeTree $
         linkedTree' (Node (Link "b") []) [(Link "b", ConstraintId "B")])
  describe "asTree" $
    it "singleton" $ do
      let tree = linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "A")]
      LinkedTree.ro (TypeTree.asTree (TypeTree tree)) `shouldBe`
        LinkedTree.ro tree
