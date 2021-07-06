module TypSpec
  ( spec
  ) where

import qualified Data.IntMap       as IM
import qualified Data.Set          as S
import           Data.Tree
import           Test.Hspec
import           Typiara.FT
import           Typiara.SampleTyp
import           Typiara.Typ

-- | Build a Typ. The first item is assumed to be the root.
t :: [(Int, FT SampleTyp Int)] -> Typ SampleTyp
t tvs = unwrap (typ root tvs')
  where
    unwrap (Right x) = x
    root = fst (head tvs)
    tvs' = IM.fromList tvs

spec :: Spec
spec = do
  describe "typ" $ do
    it "refreshes ids" $ do
      let x = t [(11, F 12 13), (12, T Num), (13, Nil)]
      tvs x `shouldBe` [(0, F 1 2), (1, T Num), (2, Nil)]
  describe "Eq" $ do
    it "same" $ do
      let x = t [(0, F 1 2), (1, T Num), (2, Nil)]
      x == x `shouldBe` True
    it "different ids, same content" $ do
      let x = t [(0, F 1 2), (1, T Num), (2, Nil)]
      let y = t [(0, F 2 3), (2, T Num), (3, Nil)]
      x `shouldBe` y
    it "different shape" $ do
      let x = t [(0, F 1 2), (1, T Num), (2, T Num)]
      let y = t [(0, Nil)]
      x == y `shouldBe` False
    it "different links, same shape" $ do
      let x = t [(0, F 1 2), (1, T Num), (2, T Num)]
      let y = t [(0, F 1 1), (1, T Num)]
      x == y `shouldBe` False
  describe "Size" $ do
    it "unlinked" $ do
      let x = t [(0, F 1 2), (1, T Num), (2, Nil)]
      size x `shouldBe` 3
    it "linked" $ do
      let x = t [(0, F 1 1), (1, Nil)]
      size x `shouldBe` 3
