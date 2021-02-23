{-# LANGUAGE OverloadedLists #-}

module Infer.TypeEnvSpec
  ( spec
  ) where

import Test.Hspec

import Typiara.Infer.FT
import Typiara.Infer.SampleTyp
import Typiara.Infer.TypeEnv

-- | Fix type to avoid ambiguity.
fixT :: TypeVarMap SampleTyp Int -> TypeVarMap SampleTyp Int
fixT = id

spec :: Spec
spec = do
  describe "findCycles" $ do
    it "singleton" $ do
      let tv = fixT [(Root, Nil)]
      findCycles tv `shouldBe` Nothing
    it "tree" $ do
      let tv = fixT [(Root, F 0 0), (NotRoot 0, Nil)]
      findCycles tv `shouldBe` Nothing
    it "diamond" $ do
      let tv =
            fixT
              [ (Root, F 0 1)
              , (NotRoot 0, T (Seq 2))
              , (NotRoot 1, T (Seq 2))
              , (NotRoot 2, Nil)
              ]
      findCycles tv `shouldBe` Nothing
    it "self cycle" $ do
      let tv = fixT [(Root, T (Seq 0)), (NotRoot 0, T (Seq 0))]
      findCycles tv `shouldBe` Just [NotRoot 0, NotRoot 0, Root]
    it "mutual cycle" $ do
      let tv =
            fixT
              [ (Root, T (Seq 0))
              , (NotRoot 0, T (Seq 1))
              , (NotRoot 1, T (Seq 0))
              ]
      findCycles tv `shouldBe` Just [NotRoot 0, NotRoot 1, NotRoot 0, Root]
