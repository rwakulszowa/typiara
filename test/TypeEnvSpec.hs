module TypeEnvSpec
  ( spec
  ) where

import qualified Data.IntMap       as IM
import           Data.Map          (fromList)
import           Data.Tree
import           Test.Hspec
import           Typiara.FT
import           Typiara.SampleTyp
import           Typiara.TypeEnv

-- | Build a TypeEnv. The first item is assumed to be the root.
te :: [(Int, FT SampleTyp Int)] -> TypeEnv SampleTyp
te tvs = TypeEnv {tvs = IM.fromList tvs, root = fst (head tvs)}

seq :: FT SampleTyp Int -> TypeEnv SampleTyp
seq t = te [(0, T (Seq 1)), (1, t)]

spec :: Spec
spec = do
  describe "fromTree" $ do
    it "singleton" $ do
      let s = Node 0 []
      let c = IM.fromList [(0, "Nil")]
      fromTree s c `shouldBe` Right (te [(0, Nil)])
    it "tree" $ do
      let s = Node 0 [Node 1 [], Node 2 [Node 3 []]]
      let c = IM.fromList [(0, "F"), (1, "Nil"), (2, "T.Seq"), (3, "T.Num")]
      fromTree s c `shouldBe`
        Right (te [(0, F 1 2), (1, Nil), (2, T (Seq 3)), (3, T Num)])
    it "missing vars" $ do
      let s = Node 0 [Node 1 []]
      let c = IM.fromList [(0, "F")]
      (fromTree s c :: Either FromTreeError (TypeEnv SampleTyp)) `shouldBe`
        Left (UntagError "F" [1])
  describe "fromEnumTree" $ do
    it "tree" $ do
      let s = Node 'a' [Node 'b' [], Node 'c' []]
      let c = fromList [('a', "F"), ('b', "Nil"), ('c', "T.Num")]
      fromEnumTree s c `shouldBe` Right (te [(0, F 1 2), (1, Nil), (2, T Num)])
    it "out of sync" $ do
      let s = Node 'x' [Node 'y' []]
      let c = fromList [('a', "T.Seq")]
      (fromEnumTree s c :: Either (FromEnumTreeError Char) (TypeEnv SampleTyp)) `shouldBe`
        Left (ShapeConstraintsOutOfSync 'a')
  describe "arityFun" $ do
    it "singleton" $ arityFun 0 `shouldBe` te [(0, Nil)]
    it "function" $
      arityFun 2 `shouldBe`
      te [(3, F 0 4), (4, F 1 2), (0, Nil), (1, Nil), (2, Nil)]
  describe "makeFun" $ do
    it "nested linked" $
      shape (makeFun [[0, 1], [1, 2, 3], [0]] :: TypeEnv SampleTyp) `shouldBe`
      shape
        (te $
         [(10, F 11 12), (11, F 0 1), (12, F 13 0), (13, F 1 14), (14, F 2 3)] <>
         [(i, Nil) | i <- [0 .. 3]])
  describe "decompose" $ do
    it "tree" $ do
      let x = te [(0, F 1 2), (1, Nil), (2, T Num)]
      decompose x `shouldBe`
        ( Node 0 [Node 1 [], Node 2 []]
        , fromList [(0, "F"), (1, "Nil"), (2, "T.Num")])
    it "tree with links" $ do
      let x = te [(0, F 1 1), (1, Nil)]
      decompose x `shouldBe`
        (Node 0 [Node 1 [], Node 1 []], fromList [(0, "F"), (1, "Nil")])
  describe "findCycles" $ do
    it "singleton" $
      -- TODO: move to a separate spec file
     do
      let m = IM.fromList [(0, [])]
      findCycles 0 m `shouldBe` Nothing
    it "tree" $ do
      let m = IM.fromList [(0, [1, 1]), (1, [])]
      findCycles 0 m `shouldBe` Nothing
    it "diamond" $ do
      let m = IM.fromList [(0, [1, 2]), (1, [3]), (2, [3]), (3, [])]
      findCycles 0 m `shouldBe` Nothing
    it "self cycle" $ do
      let m = IM.fromList [(0, [1]), (1, [1])]
      findCycles 0 m `shouldBe` Just [1, 1, 0]
    it "mutual cycle" $ do
      let m = IM.fromList [(0, [1]), (1, [2]), (2, [1])]
      findCycles 0 m `shouldBe` Just [1, 2, 1, 0]
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
      (shape <$> unifyEnv 0 x y) `shouldBe` Right (shape x)
    it "(a -> a) Num,  a" $ do
      let x = te [(0, F 1 1), (1, Nil)]
      let y = te [(0, T Num)]
      (shape <$> unifyEnv 1 x y) `shouldBe`
        Right (shape $ te [(0, F 1 1), (1, T Num)])
    it "(x -> y) (a -> a),  - propagates links" $
      -- Validate that links from the right tree are propagated to the left tree.
     do
      let x = te [(0, F 1 2), (1, Nil), (2, T Num)]
      let y = te [(0, F 1 1), (1, Nil)]
      (shape <$> unifyEnv 0 x y) `shouldBe`
        Right (shape $ te [(0, F 1 1), (1, T Num)])
    it "((x -> y) -> (y -> z)) (a -> a),  - propagates links across branches" $
      -- Validate that links from the rigt tree are propagated to the merged node,
      -- as well as the other branch.
     do
      let x =
            te
              [(0, F 1 2), (1, F 3 4), (2, F 4 5), (3, Nil), (4, Nil), (5, Nil)]
      let y = te [(0, F 1 1), (1, Nil)]
      (shape <$> unifyEnv 1 x y) `shouldBe`
        Right
          (shape $ te [(0, F 1 2), (1, F 3 3), (2, F 3 4), (3, Nil), (4, Nil)])
    it "tree" $ do
      let x = te [(0, F 1 2), (1, Nil), (2, T (Seq 3)), (3, T Num)]
      shape x `shouldBe`
        Node
          (0, "F")
          [Node (1, "Nil") [], Node (2, "T.Seq") [Node (3, "T.Num") []]]
    it "Nil constraint, 0" $ do
      let x = singleton Nil
      let y = singleton (T SampleConstraint)
      (shape <$> unifyEnv 0 x y) `shouldBe` Right (shape y)
    it "(constr -> constr) Num, 1" $ do
      let x = te [(0, F 1 1), (1, T SampleConstraint)]
      let y = singleton (T Num)
      (shape <$> unifyEnv 1 x y) `shouldBe`
        Right (shape (te [(0, F 1 1), (1, T Num)]))
    it "(constr -> constr) (Seq Num), 1; constraint propagation" $ do
      let x = te [(0, F 1 1), (1, T SampleConstraint)]
      let y = te [(0, T (Seq 1)), (1, Nil)]
      (shape <$> unifyEnv 1 x y) `shouldBe`
        Right (shape (te [(0, F 1 1), (1, T (Seq 2)), (2, T SampleConstraint)]))
