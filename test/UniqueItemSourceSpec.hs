{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UniqueItemSourceSpec
  ( spec
  ) where

import Test.Hspec

import Typiara.UniqueItemSource

spec :: Spec
spec =
  describe "uniqueItemSource" $ do
    describe "next" $ do
      it "empty list" $ do
        let (source :: UniqueItemSource Int) = fromList []
        next source `shouldBe` Left SourceDepleted
      it "nonempty" $ do
        let source = fromList [1, 2, 3]
        (fst <$> next source) `shouldBe` Right 1
    describe "takeUnique" $ do
      it "infinite list" $ do
        let source = fromList [1 ..]
        takeUnique 3 source `shouldBe` Right [1, 2, 3]
      it "duplicates" $ do
        let source = fromList [1, 2, 1]
        takeUnique 3 source `shouldBe` (Left $ Duplicate 1)
      it "out of bounds" $ do
        let source = fromList [1, 2]
        takeUnique 3 source `shouldBe` Left SourceDepleted
