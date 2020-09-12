{-# LANGUAGE OverloadedLists #-}

module TypeDefSpec
  ( spec
  ) where

import Typiara.TypeDef

import Test.Hspec

import Data.Map (Map(..))
import Data.Tree (Tree(..))

import TestConstraint (TestConstraint(..))
import Typiara.Link (Link(..))
import Typiara.LinkedTree (LinkedTree(..), linkedTree)
import Typiara.TypeTree (TypeTree(..))
import Typiara.Utils (fromRight)

linkedTree' :: Tree Link -> Map Link a -> LinkedTree a
linkedTree' s v = fromRight $ linkedTree s v

spec :: Spec
spec =
  describe "intoTypeTree" $ do
    it "singleton" $
      intoTypeTree (TypeDef (Node "a" []) [("a", ConstraintId "C0")]) `shouldBe`
      Right
        (TypeTree $
         linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "C0")])
    it "straight path" $
      intoTypeTree
        (TypeDef
           (Node "root" [Node "0" [Node "0-0" []]])
           [ ("root", ConstraintId "C0")
           , ("0", ConstraintId "C1")
           , ("0-0", ConstraintId "C2")
           ]) `shouldBe`
      (Right $
       TypeTree
         (linkedTree'
            (Node (Link "root") [Node (Link "0") [Node (Link "0-0") []]])
            [ (Link "root", ConstraintId "C0")
            , (Link "0", ConstraintId "C1")
            , (Link "0-0", ConstraintId "C2")
            ]))
    it "wide, flat" $
      intoTypeTree
        (TypeDef
           (Node "root" [Node "0" [], Node "1" []])
           [ ("root", ConstraintId "C0")
           , ("0", ConstraintId "C1")
           , ("1", ConstraintId "C2")
           ]) `shouldBe`
      (Right $
       TypeTree
         (linkedTree'
            (Node (Link "root") [Node (Link "0") [], Node (Link "1") []])
            [ (Link "root", ConstraintId "C0")
            , (Link "0", ConstraintId "C1")
            , (Link "1", ConstraintId "C2")
            ]))
