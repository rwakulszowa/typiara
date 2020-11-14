{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module DagSpec
  ( spec
  ) where

import Typiara.Dag

import Test.Hspec

import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Tree (Tree(..))

import qualified Typiara.UniqueItemSource as UniqueItemSource

import Typiara.UniqueItemSource (UniqueItemSource)
import Typiara.Utils (fromRight)

fromTree' :: (Ord id, Show id) => Tree id -> Dag id
fromTree' = fromRight . fromTree

spec :: Spec
spec = do
  describe "fromTree" $ do
    it "singleton" $
      fromTree (Node 'A' []) `shouldBe` (Right $ Dag 'A' Map.empty)
    it "simple tree" $
      fromTree (Node 'A' [Node 'B' [], Node 'C' []]) `shouldBe`
      (Right $ Dag 'A' [(EdgeSrc 'A' 0, 'B'), (EdgeSrc 'A' 1, 'C')])
    it "simple tree with links" $
      fromTree (Node 'A' [Node 'B' [], Node 'B' []]) `shouldBe`
      (Right $ Dag 'A' [(EdgeSrc 'A' 0, 'B'), (EdgeSrc 'A' 1, 'B')])
    it "simple tree with links on different levels" $
      fromTree (Node 'A' [Node 'B' [], Node 'C' [Node 'B' []]]) `shouldBe`
      (Right $
       Dag
         'A'
         [(EdgeSrc 'A' 0, 'B'), (EdgeSrc 'A' 1, 'C'), (EdgeSrc 'C' 0, 'B')])
    it "cycle" $
      fromTree (Node 'A' [Node 'A' []]) `shouldBe` (Left $ Inconsistency 'A')
    it "inconsistency - leaf vs node" $
      fromTree (Node 'A' [Node 'B' [], Node 'B' [Node 'C' []]]) `shouldBe`
      (Left $ Inconsistency 'B')
    it "inconsistency - differing child" $
      fromTree (Node 'A' [Node 'B' [Node 'C' []], Node 'B' [Node 'D' []]]) `shouldBe`
      (Left $ Inconsistency 'B')
  describe "detectCycles" $ do
    it "empty" $ detectCycles (empty 'A') `shouldBe` Nothing
    it "straight path" $
      detectCycles (Dag 'A' [(EdgeSrc 'A' 0, 'B')]) `shouldBe` Nothing
    it "cyclic" $
      detectCycles (Dag 'A' [(EdgeSrc 'A' 0, 'B'), (EdgeSrc 'B' 0, 'A')]) `shouldBe`
      (Just "ABA")
  describe "merge" $ do
    let (idSource :: UniqueItemSource Char) = UniqueItemSource.fromList ['a' ..]
    it "empty empty []" $
      merge idSource (empty 'A') [] (empty 'A') `shouldBe`
      Right (empty 'a', [(Left 'A', 'a'), (Right 'A', 'a')])
    it "simple simple [] no links" $ do
      let x = fromTree' (Node 'A' [Node 'B' [], Node 'C' []])
      let y = fromTree' (Node 'D' [Node 'E' [], Node 'F' []])
      merge idSource x [] y `shouldBe`
        Right
          ( fromTree' (Node 'a' [Node 'b' [], Node 'c' []])
          , [ (Left 'A', 'a')
            , (Left 'B', 'b')
            , (Left 'C', 'c')
            , (Right 'D', 'a')
            , (Right 'E', 'b')
            , (Right 'F', 'c')
            ])
    it "exact subtree with offset" $ do
      let x = fromTree' (Node 'A' [Node 'B' []])
      let y = fromTree' (Node 'A' [])
      merge idSource x [0] y `shouldBe`
        Right
          ( fromTree' (Node 'a' [Node 'b' []])
          , [(Left 'A', 'a'), (Left 'B', 'b'), (Right 'A', 'b')])
    it "linked simple []" $
      -- Links from `x` propagate to `y`.
     do
      let x = fromTree' (Node 'A' [Node 'B' [], Node 'B' []])
      let y = fromTree' (Node 'C' [Node 'D' [], Node 'E' []])
      merge idSource x [] y `shouldBe`
        Right
          ( fromTree' (Node 'a' [Node 'b' [], Node 'b' []])
          , [ (Left 'A', 'a')
            , (Left 'B', 'b')
            , (Right 'C', 'a')
            , (Right 'D', 'b')
            , (Right 'E', 'b')
            ])
    it "simple simple [] with links; exact match" $ do
      let x = fromTree' (Node 'A' [Node 'B' [], Node 'B' []])
      let y = fromTree' (Node 'C' [Node 'D' [], Node 'D' []])
      merge idSource x [] y `shouldBe`
        Right
          ( fromTree' (Node 'a' [Node 'b' [], Node 'b' []])
          , [ (Left 'A', 'a')
            , (Left 'B', 'b')
            , (Right 'C', 'a')
            , (Right 'D', 'b')
            ])
    it "simple simple [1] with links" $ do
      let x = fromTree' (Node 'A' [Node 'B' [], Node 'B' []])
      let y = fromTree' (Node 'C' [Node 'D' [], Node 'D' []])
      merge idSource x [1] y `shouldBe`
        Right
          ( fromTree'
              (Node
                 'a'
                 [ Node 'b' [Node 'c' [], Node 'c' []]
                 , Node 'b' [Node 'c' [], Node 'c' []]
                 ])
          , [ (Left 'A', 'a')
            , (Left 'B', 'b')
            , (Right 'C', 'b')
            , (Right 'D', 'c')
            ])
    it "rejects arguments forming a cycle" $ do
      let x = fromTree' (Node 'A' [Node 'B' [], Node 'B' []])
      let y = fromTree' (Node 'C' [Node 'D' [], Node 'E' [Node 'D' []]])
      merge idSource x [] y `shouldBe` Left Cycle
