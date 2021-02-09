{-# LANGUAGE OverloadedLists, ScopedTypeVariables #-}

module Infer.ExpressionSpec
  ( spec
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Test.Hspec

import Typiara.Infer.Application
import Typiara.Infer.Expression
import Typiara.Infer.FT
import Typiara.Infer.SampleTyp
import Typiara.Infer.TypeEnv

-- | Fixed type variant to avoid ambiguous inference failures.
singleton' :: FT SampleTyp Int -> TypeEnv SampleTyp Int
singleton' = singleton

spec :: Spec
spec = do
  describe "inferExpression" $ do
    it "Num" $ do
      let ts = [(ref "a", singleton' (T Num))]
      let expr = Expression {args = [], application = (ref "a" :| [])}
      inferExpression ts expr `shouldBe` (Right (singleton' (T Num)))
    it "(Num -> Num) | a" $ do
      let ts =
            [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
            , (ref "a", singleton' Nil)
            ]
      let expr = Expression {args = [], application = (ref "f" :| [ref "a"])}
      inferExpression ts expr `shouldBe` (Right (singleton' (T Num)))
    it "a => (Num -> Num) | a" $ do
      let ts =
            [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
            , (arg "a", singleton' Nil)
            ]
      let expr =
            Expression {args = [Arg "a"], application = (ref "f" :| [arg "a"])}
      inferExpression ts expr `shouldBe`
        (Right (TypeEnv [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, T Num)]))
    it "f => f | Num" $ do
      let ts = [(arg "f", singleton' Nil), (ref "a", singleton' (T Num))]
      let expr =
            Expression {args = [Arg "f"], application = (arg "f" :| [ref "a"])}
      inferExpression ts expr `shouldBe`
        (Right
           (TypeEnv
              [ (Root, F 0 1)
              , (NotRoot 0, F 2 3)
              , (NotRoot 1, Nil)
              , (NotRoot 2, T Num)
              , (NotRoot 3, Nil)
              ]))
    it "f a => f | a" $ do
      let ts = [(arg "f", (singleton' Nil)), (arg "a", singleton' Nil)]
      let expr =
            Expression
              {args = [Arg "f", Arg "a"], application = (arg "f" :| [arg "a"])}
      inferExpression ts expr `shouldBe`
        (Right
           (TypeEnv
              [ (Root, F 0 1)
              , (NotRoot 0, F 2 3)
              , (NotRoot 1, F 5 6)
              , (NotRoot 2, Nil)
              -- 4 missing to match the exact return value.
              -- TODO: custom `Eq` implementation, ignoring id values.
              , (NotRoot 3, Nil)
              , (NotRoot 5, Nil)
              , (NotRoot 6, Nil)
              ]))
