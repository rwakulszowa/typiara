{-# LANGUAGE OverloadedLists #-}

module TypeEnvSpec
  ( spec
  ) where

import           Data.Tree
import           Test.Hspec
import           Typiara.FT
import           Typiara.SampleTyp
import           Typiara.TypeEnv

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
  describe "refreshTypeEnv" $ do
    it "tree" $
      -- | Regular `==` comparison ignores ids. Support it by directly comparing
      -- variable names.
     do
      let x =
            TypeEnv (fixT [(Root, F 4 2), (NotRoot 4, Nil), (NotRoot 2, T Num)])
      let freshX = refreshTypeEnv x
      let y =
            TypeEnv (fixT [(Root, F 0 1), (NotRoot 0, Nil), (NotRoot 1, T Num)])
      (freshX, allVars (unTypeEnv freshX)) `shouldBe` (y, [0, 1])
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
  describe "unifyEnv" $ do
    it "Nil Nil, Root" $ do
      let x = TypeEnv (fixT [(Root, Nil)])
      let y = TypeEnv (fixT [(Root, Nil)])
      unifyEnv Root x y `shouldBe` Right x
    it "(a -> a) Num, NotRoot a" $ do
      let x = TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, Nil)])
      let y = TypeEnv (fixT [(Root, T Num)])
      unifyEnv (NotRoot 0) x y `shouldBe`
        Right (TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, T Num)]))
    it "(x -> y) (a -> a), NotRoot - propagates links" $
      -- Validate that links from the right tree are propagated to the left tree.
     do
      let x =
            TypeEnv (fixT [(Root, F 0 1), (NotRoot 0, Nil), (NotRoot 1, T Num)])
      let y = TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, Nil)])
      unifyEnv Root x y `shouldBe`
        Right (TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, T Num)]))
    it
      "((x -> y) -> (y -> z)) (a -> a), NotRoot - propagates links across branches" $
      -- Validate that links from the rigt tree are propagated to the merged node,
      -- as well as the other branch.
     do
      let x =
            TypeEnv
              (fixT
                 [ (Root, F 0 1)
                 , (NotRoot 0, F 2 3)
                 , (NotRoot 1, F 3 4)
                 , (NotRoot 2, Nil)
                 , (NotRoot 3, Nil)
                 , (NotRoot 4, Nil)
                 ])
      let y = TypeEnv (fixT [(Root, F 0 0), (NotRoot 0, Nil)])
      unifyEnv (NotRoot 0) x y `shouldBe`
        Right
          (TypeEnv
             (fixT
                [ (Root, F 0 1)
                , (NotRoot 0, F 2 2)
                , (NotRoot 1, F 2 3)
                , (NotRoot 2, Nil)
                , (NotRoot 3, Nil)
                ]))
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
