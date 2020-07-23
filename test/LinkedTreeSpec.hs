{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinkedTreeSpec
  ( spec
  ) where

import LinkedTree

import Test.Hspec

import qualified Data.Map.Strict as Map

import Data.List.NonEmpty (NonEmpty(..))

import Data.Either (isLeft)
import Data.Foldable (foldl, sum)
import Data.Tree

import qualified Dag

import Link

spec :: Spec
spec = do
  describe "fmap" $ do
    it "singleton" $
      (+ 1) <$>
      LinkedTree (Node (Link "A") []) [(Link "A", 1)] `shouldBe`
      LinkedTree (Node (Link "A") []) [(Link "A", 2)]
    it "nested" $
      (+ 10) <$>
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
        [(Link "A", 1), (Link "B", 2), (Link "C", 3)] `shouldBe`
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
        [(Link "A", 11), (Link "B", 12), (Link "C", 13)]
    it "with LinkedNode" $
      (+ 10) <$>
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
        [(Link "A", 1), (Link "B", 2)] `shouldBe`
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
        [(Link "A", 11), (Link "B", 12)]
  describe "scan" $ do
    it "singleton" $
      sum `scan` LinkedTree (Node (Link "A") []) [(Link "A", 1)] `shouldBe`
      LinkedTree (Node (Link "A") []) [(Link "A", 1)]
    it "nested" $
      sum `scan`
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
        [(Link "A", 1), (Link "B", 2), (Link "C", 3)] `shouldBe`
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
        [(Link "A", 6), (Link "B", 2), (Link "C", 3)]
    it "with LinkedNode" $
      sum `scan`
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
        [(Link "A", 1), (Link "B", 2)] `shouldBe`
      LinkedTree
        (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
        [(Link "A", 5), (Link "B", 2)]
  describe "foldl" $ do
    it "singleton" $
      concat (LinkedTree (Node (Link "A") []) [(Link "A", "a")]) `shouldBe` "a"
    it "nested" $
      concat
        (LinkedTree
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", "a"), (Link "B", "b"), (Link "C", "c")]) `shouldBe`
      "abc"
    it "with LinkedNode" $
      concat
        (LinkedTree
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", "a"), (Link "B", "b")]) `shouldBe`
      "ab"
  describe "sequence" $ do
    it "singleton - Just" $
      sequence (LinkedTree (Node (Link "A") []) [(Link "A", Just "a")]) `shouldBe`
      Just (LinkedTree (Node (Link "A") []) [(Link "A", "a")])
    it "singleton - Nothing" $
      sequence
        (LinkedTree (Node (Link "A") []) [(Link "A", Nothing :: Maybe Char)]) `shouldBe`
      Nothing
    it "nested, mixed" $
      sequence
        (LinkedTree
           (Node (Link "A") [Node (Link "B") []])
           [(Link "A", Just "a"), (Link "B", Nothing)]) `shouldBe`
      Nothing
  describe "intoTree" $ do
    it "singleton" $
      intoTree (LinkedTree (Node (Link "A") []) [(Link "A", 1)]) `shouldBe`
      Node (Link "A", 1) []
    it "nested, no links" $
      intoTree
        (LinkedTree
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", 1), (Link "B", 2), (Link "C", 3)]) `shouldBe`
      Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "C", 3) []]
    it "nested, links" $
      intoTree
        (LinkedTree
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)]) `shouldBe`
      Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "B", 2) []]
    it "complex, multiple links on different levels" $
      intoTree
        (LinkedTree
           (Node
              (Link "A")
              [ Node (Link "B") [Node (Link "C") [], Node (Link "C") []]
              , Node (Link "C") []
              ])
           [(Link "A", 1), (Link "B", 2), (Link "C", 3)]) `shouldBe`
      Node
        (Link "A", 1)
        [ Node (Link "B", 2) [Node (Link "C", 3) [], Node (Link "C", 3) []]
        , Node (Link "C", 3) []
        ]
  describe "fromTree" $ do
    it "singleton" $
      fromTree (Node (Link "A", 1) []) `shouldBe`
      (Right $ LinkedTree (Node (Link "A") []) [(Link "A", 1)])
    it "nested, links" $
      fromTree
        (Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "B", 2) []]) `shouldBe`
      (Right $
       LinkedTree
         (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
         [(Link "A", 1), (Link "B", 2)])
    it "links with children" $
      fromTree
        (Node
           (Link "A", 1)
           [ Node (Link "B", 2) [Node (Link "C", 3) []]
           , Node (Link "B", 2) [Node (Link "C", 3) []]
           ]) `shouldBe`
      (Right $
       LinkedTree
         (Node
            (Link "A")
            [ Node (Link "B") [Node (Link "C") []]
            , Node (Link "B") [Node (Link "C") []]
            ])
         [(Link "A", 1), (Link "B", 2), (Link "C", 3)])
    it "conflicting values" $
      fromTree
        (Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "B", 3) []]) `shouldBe`
      (Left $ ConflictingValues (Link "B") (2, 3))
  describe "merge" $ do
    it "singleton [] singleton" $ do
      let host = LinkedTree (Node (Link "a") []) [(Link "a", 1)]
      let guest = LinkedTree (Node (Link "a") []) [(Link "a", 2)]
      let offset = []
      merge host offset guest `shouldBe`
        (Right $ LinkedTree (Node (Link "a") []) [(Link "a", 2 :| [1])])
    it "tree offset tree" $ do
      let host =
            LinkedTree
              (Node (Link "a") [Node (Link "b") [], Node (Link "c") []])
              [(Link "a", 1), (Link "b", 2), (Link "c", 3)]
      let guest =
            LinkedTree
              (Node (Link "d") [Node (Link "e") [], Node (Link "f") []])
              [(Link "d", 4), (Link "e", 5), (Link "f", 6)]
      let offset = [1]
      merge host offset guest `shouldBe`
        (Right $
         LinkedTree
           (Node
              (Link "a")
              [ Node (Link "b") []
              , Node (Link "c") [Node (Link "d") [], Node (Link "e") []]
              ])
           [ (Link "a", 1 :| [])
           , (Link "b", 2 :| [])
           , (Link "c", 4 :| [3])
           , (Link "d", 5 :| [])
           , (Link "e", 6 :| [])
           ])
    it "at linked node" $ do
      let host =
            LinkedTree
              (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
              [(Link "a", 1), (Link "b", 2)]
      let guest =
            LinkedTree
              (Node (Link "c") [Node (Link "d") [], Node (Link "d") []])
              [(Link "c", 3), (Link "d", 4)]
      let offset = [1]
      merge host offset guest `shouldBe`
        (Right $
         LinkedTree
           (Node
              (Link "a")
              [ Node (Link "b") [Node (Link "c") [], Node (Link "c") []]
              , Node (Link "b") [Node (Link "c") [], Node (Link "c") []]
              ])
           [(Link "a", 1 :| []), (Link "b", 3 :| [2]), (Link "c", 4 :| [])])
    it "at linked nodes' parent copies children across linked branches" $ do
      let host =
            LinkedTree
              (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
              [(Link "a", 1), (Link "b", 2)]
      let guest =
            LinkedTree
              (Node (Link "c") [Node (Link "d") [Node (Link "e") []]])
              [(Link "c", 3), (Link "d", 4), (Link "e", 5)]
      let offset = []
      merge host offset guest `shouldBe`
        (Right $
         LinkedTree
           (Node
              (Link "a")
              [ Node (Link "b") [Node (Link "c") []]
              , Node (Link "b") [Node (Link "c") []]
              ])
           [(Link "a", 3 :| [1]), (Link "b", 4 :| [2]), (Link "c", 5 :| [])])
    it "invalid offset" $ do
      let host = LinkedTree (Node (Link "a") []) [(Link "a", 1)]
      let guest = LinkedTree (Node (Link "a") []) [(Link "a", 2)]
      let offset = [0, 0]
      merge host offset guest `shouldBe` (Left $ DagMergeErr Dag.PathNotFound)
