{-# LANGUAGE OverloadedLists #-}

module TypeEnvSpec
  ( spec
  ) where

import           Data.Map
import           Data.Tree
import           Test.Hspec
import           Typiara.FT
import           Typiara.SampleTyp
import           Typiara.TypeEnv

-- | Build a TypeEnv. The first item is assumed to be the root.
te :: [(Int, FT SampleTyp Int)] -> TypeEnv SampleTyp Int
te tvs = TypeEnv {tvs = fromList tvs, root = fst (head tvs)}

spec :: Spec
spec = do
  describe "Eq" $ do
    it "same" $ do
      let x = te [(0, F 1 2), (1, T Num), (2, Nil)]
      x == x `shouldBe` True
    it "different ids, same content" $ do
      let x = te [(0, F 1 2), (1, T Num), (2, Nil)]
      let y = te [(0, F 2 3), (2, T Num), (3, Nil)]
      x `shouldBe` y
    it "different shape" $ do
      let x = te [(0, F 1 2), (1, T Num), (2, T Num)]
      let y = te [(0, Nil)]
      x == y `shouldBe` False
    it "different links, same shape" $ do
      let x = te [(0, F 1 2), (1, T Num), (2, T Num)]
      let y = te [(0, F 1 1), (1, T Num)]
      x == y `shouldBe` False
  describe "fromTree" $ do
    it "singleton" $ do
      let s = Node 0 []
      let c = [(0, "Nil")]
      fromTree s c `shouldBe` Right (te [(0, Nil)])
    it "tree" $ do
      let s = Node 0 [Node 1 [], Node 2 [Node 3 []]]
      let c = [(0, "F"), (1, "Nil"), (2, "T.Seq"), (3, "T.Num")]
      fromTree s c `shouldBe`
        Right (te [(0, F 1 2), (1, Nil), (2, T (Seq 3)), (3, T Num)])
    it "missing vars" $ do
      let s = Node 0 [Node 1 []]
      let c = [(0, "F")]
      (fromTree s c :: Either (FromTreeError Int) (TypeEnv SampleTyp Int)) `shouldBe`
        Left (UntagError "F" [1])
  describe "fromEnumTree" $ do
    it "tree" $ do
      let s = Node 'a' [Node 'b' [], Node 'c' []]
      let c = [('a', "F"), ('b', "Nil"), ('c', "T.Num")]
      fromEnumTree s c `shouldBe` Right (te [(0, F 1 2), (1, Nil), (2, T Num)])
    it "out of sync" $ do
      let s = Node 'x' [Node 'y' []]
      let c = [('a', "T.Seq")]
      (fromEnumTree s c :: Either (FromEnumTreeError Char Int) (TypeEnv SampleTyp Int)) `shouldBe`
        Left (ShapeConstraintsOutOfSync 'a')
  describe "decompose" $ do
    it "tree" $ do
      let x = te [(0, F 1 2), (1, Nil), (2, T Num)]
      decompose x `shouldBe`
        (Node 0 [Node 1 [], Node 2 []], [(0, "F"), (1, "Nil"), (2, "T.Num")])
    it "tree with links" $ do
      let x = te [(0, F 1 1), (1, Nil)]
      decompose x `shouldBe`
        (Node 0 [Node 1 [], Node 1 []], [(0, "F"), (1, "Nil")])
  describe "refreshTypeEnv" $ do
    it "tree" $
      -- Regular `==` comparison ignores ids. Support it by directly comparing
      -- variable names.
     do
      let x = te [(0, F 5 3), (5, Nil), (3, T Num)]
      let freshX = refreshTypeEnv x
      let y = te [(0, F 2 1), (2, Nil), (1, T Num)]
      (freshX, allVars (tvs freshX)) `shouldBe` (y, [0, 1, 2])
  describe "findCycles" $ do
    it "singleton" $
      -- TODO: move to a separate spec file
     do
      let m = [('a', "")]
      findCycles 'a' m `shouldBe` Nothing
    it "tree" $ do
      let m = [('a', "bb"), ('b', "")]
      findCycles 'a' m `shouldBe` Nothing
    it "diamond" $ do
      let m = [('a', "bc"), ('b', "d"), ('c', "d"), ('d', "")]
      findCycles 'a' m `shouldBe` Nothing
    it "self cycle" $ do
      let m = [('a', "b"), ('b', "b")]
      findCycles 'a' m `shouldBe` Just "bba"
    it "mutual cycle" $ do
      let m = [('a', "b"), ('b', "c"), ('c', "b")]
      findCycles 'a' m `shouldBe` Just "bcba"
  describe "shape" $ do
    it "singleton" $ do
      let x = te [(0, Nil)]
      shape x `shouldBe` Node (0, "Nil") []
    it "tree" $ do
      let x = te [(0, F 1 2), (1, Nil), (2, T (Seq 3)), (3, T Num)]
      shape x `shouldBe`
        Node
          (0, "F")
          [Node (1, "Nil") [], Node (2, "T.Seq") [Node (3, "T.Num") []]]
  describe "unifyEnv" $ do
    it "Nil Nil, 0" $ do
      let x = te [(0, Nil)]
      let y = te [(0, Nil)]
      unifyEnv 0 x y `shouldBe` Right x
    it "(a -> a) Num,  a" $ do
      let x = te [(0, F 1 1), (1, Nil)]
      let y = te [(0, T Num)]
      unifyEnv 1 x y `shouldBe` Right (te [(0, F 1 1), (1, T Num)])
    it "(x -> y) (a -> a),  - propagates links" $
      -- Validate that links from the right tree are propagated to the left tree.
     do
      let x = te [(0, F 1 2), (1, Nil), (2, T Num)]
      let y = te [(0, F 1 1), (1, Nil)]
      unifyEnv 0 x y `shouldBe` Right (te [(0, F 1 1), (1, T Num)])
    it "((x -> y) -> (y -> z)) (a -> a),  - propagates links across branches" $
      -- Validate that links from the rigt tree are propagated to the merged node,
      -- as well as the other branch.
     do
      let x =
            te
              [(0, F 1 2), (1, F 3 4), (2, F 4 5), (3, Nil), (4, Nil), (5, Nil)]
      let y = te [(0, F 1 1), (1, Nil)]
      unifyEnv 1 x y `shouldBe`
        Right (te [(0, F 1 2), (1, F 3 3), (2, F 3 4), (3, Nil), (4, Nil)])
    it "tree" $ do
      let x = te [(0, F 1 2), (1, Nil), (2, T (Seq 3)), (3, T Num)]
      shape x `shouldBe`
        Node
          (0, "F")
          [Node (1, "Nil") [], Node (2, "T.Seq") [Node (3, "T.Num") []]]
