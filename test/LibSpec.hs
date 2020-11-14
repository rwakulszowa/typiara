{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LibSpec
  ( spec
  ) where

import Test.Hspec

import qualified Data.Map.Strict as Map

import Data.Set (Set)
import Data.Tree (Tree(..))

import qualified Typiara.Apply as Apply
import qualified Typiara.LinkedTree as LinkedTree
import qualified Typiara.TypeDef as TypeDef
import qualified Typiara.TypeTree as TypeTree

import Typiara.Example.SimpleType
import Typiara.Link (Link(..))
import Typiara.LinkedTree (LinkedTree(..))
import Typiara.TypeDef (TypeDef(..))
import Typiara.TypeTree (TypeTree(..))
import Typiara.Utils (fromRight)

spec :: Spec
spec =
  describe "merge" $
    -- Int
   do
    let intDef :: TypeDef (Set SimpleType) =
          TypeDef (Node "root" []) (Map.fromList [("root", [RigidType Int])])
    -- [Int]
    let seqDef :: TypeDef (Set SimpleType) =
          TypeDef
            (Node "root" [Node "x" []])
            (Map.fromList [("root", [RigidType Seq]), ("x", [RigidType Int])])
    -- Int -> Int
    let incDef :: TypeDef (Set SimpleType) =
          TypeDef
            (Node "root" [Node "x" [], Node "x" []])
            (Map.fromList [("root", [RigidType Fun]), ("x", [RigidType Int])])
    -- [a] -> a
    let headDef =
          TypeDef
            (Node "root" [Node "seq" [Node "x" []], Node "x" []])
            (Map.fromList
               [("root", [RigidType Fun]), ("seq", [RigidType Seq]), ("x", [])])
    -- a -> [a]
    let consDef =
          TypeDef
            (Node "root" [Node "x" [], Node "seq" [Node "x" []]])
            (Map.fromList
               [("root", [RigidType Fun]), ("seq", [RigidType Seq]), ("x", [])])
      -- (b -> c) -> (a -> b) -> (a -> c)
    let composeDef =
          TypeDef
            (Node
               "root"
               [ Node "fbc" [Node "b" [], Node "c" []]
               , Node
                   "f1node"
                   [ Node "fab" [Node "a" [], Node "b" []]
                   , Node "fac" [Node "a" [], Node "c" []]
                   ]
               ])
            (Map.fromList
               [ ("root", [RigidType Fun])
               , ("fbc", [RigidType Fun])
               , ("fab", [RigidType Fun])
               , ("fac", [RigidType Fun])
               , ("f1node", [RigidType Fun])
               , ("a", [])
               , ("b", [])
               , ("c", [])
               ])
    let (Right [int, seq, inc, compose, head, cons]) =
          mapM
            TypeDef.intoTypeTree
            ([intDef, seqDef, incDef, composeDef, headDef, consDef] :: [TypeDef (Set SimpleType)])
      -- functions with arguments flipped to aid composition.
    let merge' guest path host = TypeTree.mergeAt host path guest
    let apply' arg fun =
          Apply.appliedRet <$> arg `Apply.apply` Apply.applied fun
    describe "merge" $ do
      it "inc . head" $
        (pure compose >>= merge' inc [0] >>= merge' head [1, 0]) `shouldBe`
        Right
          (TypeTree . fromRight $
           LinkedTree.linkedTree
             (Node
                (Link "Root")
                [ Node (Link "Inc") [Node (Link "a") [], Node (Link "a") []]
                , Node
                    (Link "F1")
                    [ Node
                        (Link "Head")
                        [ Node (Link "seq") [Node (Link "a") []]
                        , Node (Link "a") []
                        ]
                    , Node
                        (Link "Ret")
                        [ Node (Link "seq") [Node (Link "a") []]
                        , Node (Link "a") []
                        ]
                    ]
                ])
             (Map.fromList
                [ (Link "Root", [RigidType Fun])
                , (Link "Inc", [RigidType Fun])
                , (Link "Head", [RigidType Fun])
                , (Link "Ret", [RigidType Fun])
                , (Link "F1", [RigidType Fun])
                , (Link "seq", [RigidType Seq])
                , (Link "a", [RigidType Int])
                ]))
      describe "apply" $ do
        it "(inc . head) $ seq" $
          (pure compose >>= apply' inc >>= apply' head >>= apply' seq) `shouldBe`
          Right int
        it "(cons . inc) $ int" $
          (pure compose >>= apply' cons >>= apply' inc >>= apply' int) `shouldBe`
          Right seq
