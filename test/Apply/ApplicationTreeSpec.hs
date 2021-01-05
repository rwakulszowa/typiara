{-# LANGUAGE ScopedTypeVariables #-}

module Apply.ApplicationTreeSpec
  ( spec
  ) where

import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Data.Monoid (Sum(..))
import Test.Hspec
import Typiara.Apply.ApplicationTree

spec :: Spec
spec = do
  describe "traverseWithReplacement" $ do
    it "id with counter" $ do
      let tree =
            Application
              (Unapplied 'a')
              (Application (Unapplied 'b') (Unapplied 'c'))
      let fun t = do
            tell (1 :: Sum Int)
            return t
      (runWriter $ traverseWithReplacement fun tree) `shouldBe` (tree, 5)
    it "compress small subtrees" $
        -- If a sum of tree values is small, return a single node with the values
        -- added.
        -- Popped values are stored in the state.
     do
      let tree :: ApplicationTree Int =
            Application
              (Application (Unapplied 1) (Unapplied 1))
              (Application (Unapplied 1) (Unapplied 2))
      let fun t = do
            if sum t < 5
              then do
                tell [sum t]
                return $ Unapplied (sum t)
              else return t
      (runWriter $ traverseWithReplacement fun tree) `shouldBe`
        (Application (Unapplied 2) (Unapplied 3), [1, 1, 2, 1, 2, 3])
