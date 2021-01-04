{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LinkedTreeSpec
  ( spec
  ) where

import Typiara.LinkedTree

import Test.Hspec

import qualified Data.Map.Strict as Map

import Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map(..))

import Data.Either (isLeft)
import Data.Foldable (foldl, sum)
import Data.Tree

import qualified Typiara.Dag as Dag
import qualified Typiara.Utils as Utils

import Typiara.Link

linkedTree' :: Tree Link -> Map Link a -> LinkedTree a
linkedTree' s v = Utils.fromRight $ linkedTree s v

spec :: Spec
spec = do
  describe "eq" $ do
    it "equal singletons" $ (==) (singleton 'A') (singleton 'A') `shouldBe` True
    it "different values, same shapes" $
      (==) (singleton 'A') (singleton 'B') `shouldBe` False
    it "different shapes, same values" $
      (==)
        (linkedTree'
           (Node (Link "A") [Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)])
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)]) `shouldBe`
      False
    it "linked vs unlinked" $
      (==)
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)])
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", 1), (Link "B", 2), (Link "C", 2)]) `shouldBe`
      False
    it "same shape and values, different ids" $
      (==)
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)])
        (linkedTree'
           (Node (Link "X") [Node (Link "Y") [], Node (Link "Y") []])
           [(Link "X", 1), (Link "Y", 2)]) `shouldBe`
      True
  describe "fmap" $ do
    it "singleton" $
      ro ((+ 1) <$> linkedTree' (Node (Link "A") []) [(Link "A", 1)]) `shouldBe`
      (Node (Link "A") [], [(Link "A", 2)])
    it "nested" $
      ro
        ((+ 10) <$>
         linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", 1), (Link "B", 2), (Link "C", 3)]) `shouldBe`
      ( Node (Link "A") [Node (Link "B") [], Node (Link "C") []]
      , [(Link "A", 11), (Link "B", 12), (Link "C", 13)])
    it "with LinkedNode" $
      ro
        ((+ 10) <$>
         linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)]) `shouldBe`
      ( Node (Link "A") [Node (Link "B") [], Node (Link "B") []]
      , [(Link "A", 11), (Link "B", 12)])
  describe "scan" $ do
    it "singleton" $
      ro (sum `scan` linkedTree' (Node (Link "A") []) [(Link "A", 1)]) `shouldBe`
      (Node (Link "A") [], [(Link "A", 1)])
    it "nested" $
      ro
        (sum `scan`
         linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", 1), (Link "B", 2), (Link "C", 3)]) `shouldBe`
      ( Node (Link "A") [Node (Link "B") [], Node (Link "C") []]
      , [(Link "A", 6), (Link "B", 2), (Link "C", 3)])
    it "with LinkedNode" $
      ro
        (sum `scan`
         linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)]) `shouldBe`
      ( Node (Link "A") [Node (Link "B") [], Node (Link "B") []]
      , [(Link "A", 5), (Link "B", 2)])
  describe "foldl" $ do
    it "singleton" $
      concat (linkedTree' (Node (Link "A") []) [(Link "A", "a")]) `shouldBe` "a"
    it "nested" $
      concat
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", "a"), (Link "B", "b"), (Link "C", "c")]) `shouldBe`
      "abc"
    it "with LinkedNode" $
      concat
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", "a"), (Link "B", "b")]) `shouldBe`
      "ab"
  describe "sequence" $ do
    it "singleton - Just" $
      (ro <$> sequence (linkedTree' (Node (Link "A") []) [(Link "A", Just "a")])) `shouldBe`
      Just (Node (Link "A") [], [(Link "A", "a")])
    it "singleton - Nothing" $
      (ro <$>
       sequence
         (linkedTree' (Node (Link "A") []) [(Link "A", Nothing :: Maybe Char)])) `shouldBe`
      Nothing
    it "nested, mixed" $
      (ro <$>
       sequence
         (linkedTree'
            (Node (Link "A") [Node (Link "B") []])
            [(Link "A", Just "a"), (Link "B", Nothing)])) `shouldBe`
      Nothing
  describe "intoTree" $ do
    it "singleton" $
      intoTree (linkedTree' (Node (Link "A") []) [(Link "A", 1)]) `shouldBe`
      Node (Link "A", 1) []
    it "nested, no links" $
      intoTree
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "C") []])
           [(Link "A", 1), (Link "B", 2), (Link "C", 3)]) `shouldBe`
      Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "C", 3) []]
    it "nested, links" $
      intoTree
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 1), (Link "B", 2)]) `shouldBe`
      Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "B", 2) []]
    it "complex, multiple links on different levels" $
      intoTree
        (linkedTree'
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
      (ro <$> fromTree (Node (Link "A", 1) [])) `shouldBe`
      Right (Node (Link "A") [], [(Link "A", 1)])
    it "nested, links" $
      (ro <$>
       fromTree
         (Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "B", 2) []])) `shouldBe`
      Right
        ( Node (Link "A") [Node (Link "B") [], Node (Link "B") []]
        , [(Link "A", 1), (Link "B", 2)])
    it "links with children" $
      (ro <$>
       fromTree
         (Node
            (Link "A", 1)
            [ Node (Link "B", 2) [Node (Link "C", 3) []]
            , Node (Link "B", 2) [Node (Link "C", 3) []]
            ])) `shouldBe`
      Right
        ( Node
            (Link "A")
            [ Node (Link "B") [Node (Link "C") []]
            , Node (Link "B") [Node (Link "C") []]
            ]
        , [(Link "A", 1), (Link "B", 2), (Link "C", 3)])
    it "conflicting values" $
      (ro <$>
       fromTree
         (Node (Link "A", 1) [Node (Link "B", 2) [], Node (Link "B", 3) []])) `shouldBe`
      (Left $ ConflictingValues (Link "B") (2, 3))
  describe "fromTreeImplicit" $ do
    it "nested, links" $
      fromTreeImplicit (Node 'x' [Node 'y' [], Node 'y' []]) `shouldBe`
      Right
        (linkedTree'
           (Node (Link "A") [Node (Link "B") [], Node (Link "B") []])
           [(Link "A", 'x'), (Link "B", 'y')])
  describe "merge" $ do
    it "singleton [] singleton" $ do
      let host = linkedTree' (Node (Link "a") []) [(Link "a", 1)]
      let guest = linkedTree' (Node (Link "a") []) [(Link "a", 2)]
      let offset = []
      (ro <$> merge host offset guest) `shouldBe`
        Right (Node (Link "a") [], [(Link "a", 2 :| [1])])
    it "tree offset tree" $ do
      let host =
            linkedTree'
              (Node (Link "a") [Node (Link "b") [], Node (Link "c") []])
              [(Link "a", 1), (Link "b", 2), (Link "c", 3)]
      let guest =
            linkedTree'
              (Node (Link "d") [Node (Link "e") [], Node (Link "f") []])
              [(Link "d", 4), (Link "e", 5), (Link "f", 6)]
      let offset = [1]
      (ro <$> merge host offset guest) `shouldBe`
        Right
          ( Node
              (Link "a")
              [ Node (Link "b") []
              , Node (Link "c") [Node (Link "d") [], Node (Link "e") []]
              ]
          , [ (Link "a", 1 :| [])
            , (Link "b", 2 :| [])
            , (Link "c", 4 :| [3])
            , (Link "d", 5 :| [])
            , (Link "e", 6 :| [])
            ])
    it "at linked node" $ do
      let host =
            linkedTree'
              (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
              [(Link "a", 1), (Link "b", 2)]
      let guest =
            linkedTree'
              (Node (Link "c") [Node (Link "d") [], Node (Link "d") []])
              [(Link "c", 3), (Link "d", 4)]
      let offset = [1]
      (ro <$> merge host offset guest) `shouldBe`
        Right
          ( Node
              (Link "a")
              [ Node (Link "b") [Node (Link "c") [], Node (Link "c") []]
              , Node (Link "b") [Node (Link "c") [], Node (Link "c") []]
              ]
          , [(Link "a", 1 :| []), (Link "b", 3 :| [2]), (Link "c", 4 :| [])])
    it "at linked nodes' parent copies children across linked branches" $ do
      let host =
            linkedTree'
              (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
              [(Link "a", 1), (Link "b", 2)]
      let guest =
            linkedTree'
              (Node (Link "c") [Node (Link "d") [Node (Link "e") []]])
              [(Link "c", 3), (Link "d", 4), (Link "e", 5)]
      let offset = []
      (ro <$> merge host offset guest) `shouldBe`
        Right
          ( Node
              (Link "a")
              [ Node (Link "b") [Node (Link "c") []]
              , Node (Link "b") [Node (Link "c") []]
              ]
          , [(Link "a", 3 :| [1]), (Link "b", 4 :| [2]), (Link "c", 5 :| [])])
    it "invalid offset" $ do
      let host = linkedTree' (Node (Link "a") []) [(Link "a", 1)]
      let guest = linkedTree' (Node (Link "a") []) [(Link "a", 2)]
      let offset = [0, 0]
      (ro <$> merge host offset guest) `shouldBe`
        (Left $ DagMergeErr Dag.PathNotFound)
    it "incompatible shapes - reject a cycle" $ do
      let host =
            linkedTree'
              (Node (Link "a") [Node (Link "b") [], Node (Link "b") []])
              [(Link "a", ()), (Link "b", ())]
      let guest =
            linkedTree'
              (Node
                 (Link "a")
                 [Node (Link "b") [], Node (Link "c") [Node (Link "b") []]])
              [(Link "a", ()), (Link "b", ()), (Link "c", ())]
      let offset = []
      (ro <$> merge host offset guest) `shouldBe` (Left $ DagMergeErr Dag.Cycle)
  describe "decompose" $ do
    it "singleton" $ decompose (singleton 'A') `shouldBe` ('A', [])
    it "single child" $
      decompose
        (linkedTree'
           (Node (Link "a") [Node (Link "b") []])
           [(Link "a", 'A'), (Link "b", 'B')]) `shouldBe`
      ('A', [singleton 'B'])
    it "triple" $
      decompose (triple 'A' 'B' 'C') `shouldBe`
      ('A', [singleton 'B', singleton 'C'])
    it "nested" $
      decompose
        (linkedTree'
           (Node (Link "a") [Node (Link "b") [Node (Link "c") []]])
           [(Link "a", 'A'), (Link "b", 'B'), (Link "c", 'C')]) `shouldBe`
      ( 'A'
      , [ linkedTree'
            (Node (Link "b") [Node (Link "c") []])
            [(Link "b", 'B'), (Link "c", 'C')]
        ])
  describe "arePathsLinked" $ do
    it "singleton" $ arePathsLinked [0] [1] (singleton 'A') `shouldBe` Left [0]
    it "triple" $
      arePathsLinked [0] [1] (triple 'A' 'B' 'C') `shouldBe` Right False
    it "linkedTriple" $
      arePathsLinked [0] [1] (linkedTriple 'A' 'B') `shouldBe` Right True
    it "different levels" $
      arePathsLinked
        [0, 0]
        [1]
        (linkedTree'
           (Node
              (Link "A")
              [Node (Link "B") [Node (Link "C") []], Node (Link "C") []])
           [(Link "A", 1), (Link "B", 2), (Link "C", 3)]) `shouldBe`
      Right True
