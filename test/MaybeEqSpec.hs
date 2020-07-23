module MaybeEqSpec
  ( spec
  ) where

import MaybeEq

import Test.Hspec

spec :: Spec
spec =
  describe "eq" $ do
    it "Nothings" $
      (MaybeEq Nothing :: MaybeEq Int) == MaybeEq Nothing `shouldBe` False
    it "mixed" $ MaybeEq Nothing == (MaybeEq $ Just 1) `shouldBe` False
    it "Justs equal" $ (MaybeEq $ Just 1) == (MaybeEq $ Just 1) `shouldBe` True
    it "Justs different" $
      (MaybeEq $ Just 1) == (MaybeEq $ Just 2) `shouldBe` False
