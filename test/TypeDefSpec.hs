{-# LANGUAGE OverloadedLists #-}

module TypeDefSpec
  ( spec
  ) where

import TypeDef

import Test.Hspec

import Data.Tree (Tree(..))

import Link (Link(..))
import LinkedTree (LinkedTree(..))
import TestConstraint (TestConstraint(..))
import TypeTree (TypeTree(..))

spec :: Spec
spec =
  describe "intoTypeTree" $ do
    it "singleton" $
      TypeDef.intoTypeTree (TypeDef (Node "a" []) [("a", ConstraintId "C0")]) `shouldBe`
      Right
        (TypeTree $
         LinkedTree (Node (Link "a") []) [(Link "a", ConstraintId "C0")])
    it "straight path" $
      TypeDef.intoTypeTree
        (TypeDef
           (Node "root" [Node "0" [Node "0-0" []]])
           [ ("root", ConstraintId "C0")
           , ("0", ConstraintId "C1")
           , ("0-0", ConstraintId "C2")
           ]) `shouldBe`
      (Right $
       TypeTree
         (LinkedTree
            (Node (Link "root") [Node (Link "0") [Node (Link "0-0") []]])
            [ (Link "root", ConstraintId "C0")
            , (Link "0", ConstraintId "C1")
            , (Link "0-0", ConstraintId "C2")
            ]))
    it "wide, flat" $
      TypeDef.intoTypeTree
        (TypeDef
           (Node "root" [Node "0" [], Node "1" []])
           [ ("root", ConstraintId "C0")
           , ("0", ConstraintId "C1")
           , ("1", ConstraintId "C2")
           ]) `shouldBe`
      (Right $
       TypeTree
         (LinkedTree
            (Node (Link "root") [Node (Link "0") [], Node (Link "1") []])
            [ (Link "root", ConstraintId "C0")
            , (Link "0", ConstraintId "C1")
            , (Link "1", ConstraintId "C2")
            ]))
