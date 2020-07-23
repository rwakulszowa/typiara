{-# LANGUAGE OverloadedLists #-}

module TypeSpec
  ( spec
  ) where

import Test.Hspec

import qualified Data.Set as Set

import Constraint
import Data.Either (isLeft)
import Link
import Type

spec :: Spec
spec =
  describe "mergeConstraints" $ do
    it "single rigid" $ do
      let c = Set.singleton $ RigidType Int
      mergeConstraints [c] `shouldBe` Right c
    it "rigid and requirement" $ do
      let c = Set.fromList [RigidType Int, Requirement Show]
      mergeConstraints [c] `shouldBe` Right c
    it "conflicting rigid" $ do
      let c = Set.fromList [RigidType Int, RigidType Char]
      mergeConstraints [c] `shouldSatisfy` isLeft
