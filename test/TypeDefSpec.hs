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
spec = do
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
  describe "fromTypeTree" $ do
    it "singleton" $
      fromTypeTree
        (TypeTree $
         linkedTree' (Node (Link "a") []) [(Link "a", ConstraintId "C0")]) `shouldBe`
      TypeDef (Node "a" []) [("a", ConstraintId "C0")]
    it "linked" $
      fromTypeTree
        (TypeTree $
         linkedTree'
           (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
           [(Link "a", ConstraintId "C0"), (Link "b", ConstraintId "C1")]) `shouldBe`
      TypeDef
        (Node "a" [Node "b" [], Node "b" []])
        [("a", ConstraintId "C0"), ("b", ConstraintId "C1")]
  describe "toTypeTree . fromTypeTree round trip" $
    -- | Validate that no information is lost in the transition.
    -- | May detect if some additional metadata is added to TypeTree, but not yet reflected
    -- | in TypeDef.
   do
    let round td = fromTypeTree <$> intoTypeTree td
    it "singleton" $ do
      let td = TypeDef (Node "a" []) [("a", ConstraintId "C0")]
      round td `shouldBe` Right td
    it "linked" $ do
      let td =
            TypeDef
              (Node "a" [Node "b" [], Node "b" []])
              [("a", ConstraintId "C0"), ("b", ConstraintId "C1")]
      round td `shouldBe` Right td
