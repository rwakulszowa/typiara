{-# LANGUAGE OverloadedLists #-}

module TypeEnvSpec
  ( spec
  ) where

import Data.Tree
import Test.Hspec
import Typiara.FT
import Typiara.SampleTyp
import Typiara.TypeEnv

-- | Fix type to avoid ambiguity.
fixT :: TypeVarMap SampleTyp Int -> TypeVarMap SampleTyp Int
fixT = id

spec :: Spec
spec = do
  describe "Eq" $ do
    it "same" $ do
      let te =
            TypeEnv (fixT [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, Nil)])
      te == te `shouldBe` True
    it "different ids, same content" $ do
      let x =
            TypeEnv (fixT [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, Nil)])
      let y =
            TypeEnv (fixT [(Root, F 1 2), (NotRoot 1, T Num), (NotRoot 2, Nil)])
      x `shouldBe` y
    it "different shape" $ do
      let x =
            TypeEnv
              (fixT [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, T Num)])
      let y = TypeEnv (fixT [(Root, Nil)])
      x == y `shouldBe` False
    it "different links, same shape" $ do
      let x =
            TypeEnv
              (fixT [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, T Num)])
      let y = TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, T Num)])
      x == y `shouldBe` False
  describe "fromTree" $ do
    it "singleton" $ do
      let s = Node Root []
      let c = [(Root, "Nil")]
      fromTree s c `shouldBe` Right (TypeEnv (fixT [(Root, Nil)]))
    it "tree" $ do
      let s =
            Node
              Root
              [Node (NotRoot 0) [], Node (NotRoot 1) [Node (NotRoot 2) []]]
      let c =
            [ (Root, "F")
            , (NotRoot 0, "Nil")
            , (NotRoot 1, "T.Seq")
            , (NotRoot 2, "T.Num")
            ]
      fromTree s c `shouldBe`
        Right
          (TypeEnv
             (fixT
                [ (Root, F 0 1)
                , (NotRoot 0, Nil)
                , (NotRoot 1, T (Seq 2))
                , (NotRoot 2, T Num)
                ]))
    it "missing vars" $ do
      let s = Node Root [Node (NotRoot 0) []]
      let c = [(Root, "F")]
      (fromTree s c :: Either (FromTreeError Int) (TypeEnv SampleTyp Int)) `shouldBe`
        Left (UntagError "F" [0])
  describe "fromEnumTree" $ do
    it "tree" $ do
      let s = Node 'a' [Node 'b' [], Node 'c' []]
      let c = [('a', "F"), ('b', "Nil"), ('c', "T.Num")]
      fromEnumTree s c `shouldBe`
        Right
          (TypeEnv (fixT [(Root, F 0 1), (NotRoot 0, Nil), (NotRoot 1, T Num)]))
    it "out of sync" $ do
      let s = Node 'x' [Node 'y' []]
      let c = [('a', "T.Seq")]
      (fromEnumTree s c :: Either (FromEnumTreeError Char Int) (TypeEnv SampleTyp Int)) `shouldBe`
        Left (ShapeConstraintsOutOfSync 'a')
  describe "decompose" $ do
    it "tree" $ do
      let te =
            TypeEnv (fixT [(Root, F 0 1), (NotRoot 0, Nil), (NotRoot 1, T Num)])
      decompose te `shouldBe`
        (Node 0 [Node 1 [], Node 2 []], [(0, "F"), (1, "Nil"), (2, "T.Num")])
    it "tree with links" $ do
      let te = TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, Nil)])
      decompose te `shouldBe`
        (Node 0 [Node 1 [], Node 1 []], [(0, "F"), (1, "Nil")])
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
  describe "shape" $ do
    it "singleton" $ do
      let te = TypeEnv (fixT [(Root, Nil)])
      shape te `shouldBe` Node (Root, "Nil") []
    it "tree" $ do
      let te =
            TypeEnv
              (fixT
                 [ (Root, F 0 1)
                 , (NotRoot 0, Nil)
                 , (NotRoot 1, T (Seq 2))
                 , (NotRoot 2, T Num)
                 ])
      shape te `shouldBe`
        Node
          (Root, "F")
          [ Node (NotRoot 0, "Nil") []
          , Node (NotRoot 1, "T.Seq") [Node (NotRoot 2, "T.Num") []]
          ]