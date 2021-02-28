{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module UtilsSpec
  ( spec
  ) where

import Typiara.Utils

import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree

import Data.Tree (Tree(..))

spec :: Spec
spec = do
  describe "hoistHeads" $ do
    it "null" $ hoistHeads ([] :: [([Int], Char)]) `shouldBe` []
    it "singleton" $ hoistHeads [([1], "A")] `shouldBe` [(1, [([], "A")])]
    it "varying lengths with null" $
      hoistHeads [([], "A"), ([1], "B")] `shouldBe` [(1, [([], "B")])]
    it "nested singleton" $
      hoistHeads [([1, 2, 3], "A")] `shouldBe` [(1, [([2, 3], "A")])]
    it "same heads" $
      hoistHeads [([1], "A"), ([1], "B")] `shouldBe`
      [(1, [([], "A"), ([], "B")])]
    it "multiple heads" $
      hoistHeads [([1], "A"), ([2], "B")] `shouldBe`
      [(1, [([], "A")]), (2, [([], "B")])]
    it "multiple heads nested" $
      hoistHeads [([1, 2], "A"), ([1, 3, 4], "B"), ([2, 3], "C")] `shouldBe`
      [(1, [([2], "A"), ([3, 4], "B")]), (2, [([3], "C")])]
  describe "tenumerate" $ do
    it "list" $ tenumerate "abc" `shouldBe` [(0, 'a'), (1, 'b'), (2, 'c')]
    it "tree" $
      tenumerate (Tree.Node 'a' [Tree.Node 'b' [], Tree.Node 'c' []]) `shouldBe`
      Tree.Node (0, 'a') [Tree.Node (1, 'b') [], Tree.Node (2, 'c') []]
  describe "denumerateSequence" $ do
    it "simple, out of order" $
      denumerateSequence [(1, 'B'), (0, 'A'), (2, 'C')] `shouldBe` Right "ABC"
    it "missing item" $
      denumerateSequence [(0, 'A'), (2, 'C')] `shouldBe` Left 1
  describe "pop" $ do
    it "failure" $ pop [('A', 1 :: Int)] 'B' `shouldBe` Nothing
    it "success" $
      pop [('A', 1 :: Int), ('B', 2)] 'B' `shouldBe` Just (2, [('A', 1)])
