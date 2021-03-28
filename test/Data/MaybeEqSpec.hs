module Data.MaybeEqSpec
  ( spec
  ) where

import           Typiara.Data.MaybeEq

import           Test.Hspec

spec :: Spec
spec = do
  describe "eq" $ do
    it "Nothings" $
      (MaybeEq Nothing :: MaybeEq Int) == MaybeEq Nothing `shouldBe` False
    it "mixed" $ MaybeEq Nothing == MaybeEq (Just 1) `shouldBe` False
    it "Justs equal" $ MaybeEq (Just 1) == MaybeEq (Just 1) `shouldBe` True
    it "Justs different" $ MaybeEq (Just 1) == MaybeEq (Just 2) `shouldBe` False
  describe "fillMaybeEq" $ do
    it "list" $
      fillMaybeEq (MaybeEq <$> [Just 'a', Nothing, Just 'b', Nothing]) `shouldBe`
      [Right 'a', Left 0, Right 'b', Left 1]
