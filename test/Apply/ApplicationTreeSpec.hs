{-# LANGUAGE OverloadedLists #-}

module Apply.ApplicationTreeSpec
  ( spec
  ) where

import Test.Hspec
import Typiara.Apply.ApplicationTree

spec :: Spec
spec = do
  describe "traverseAccumWithReplacement" $ do
    it "id with counter" $ do
      let tree =
            Application
              (Unapplied 'a')
              (Application (Unapplied 'b') (Unapplied 'c'))
      traverseAccumWithReplacement (\s t -> (s + 1, t)) 0 tree `shouldBe`
        (5, tree)
    it "compress small subtrees" $
        -- If a sum of tree values is small, return a single node with the values
        -- added.
        -- Popped values are stored in the state.
     do
      let tree =
            Application
              (Application (Unapplied 1) (Unapplied 1))
              (Application (Unapplied 1) (Unapplied 2))
      let fun state tree =
            if sum tree < 5
              then ((sum tree) : state, Unapplied (sum tree))
              else (state, tree)
      traverseAccumWithReplacement fun [] tree `shouldBe`
        ([3, 2, 1, 2, 1, 1], Application (Unapplied 2) (Unapplied 3))
