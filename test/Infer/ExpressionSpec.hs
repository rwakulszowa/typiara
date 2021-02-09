{-# LANGUAGE OverloadedLists, ScopedTypeVariables #-}

module Infer.ExpressionSpec
  ( spec
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Test.Hspec

import Typiara.Infer.Application
import Typiara.Infer.FT
import Typiara.Infer.Expression
import Typiara.Infer.TypeEnv
import Typiara.Infer.SampleTyp

spec :: Spec
spec = do
  describe "inferExpression" $ do
    it "Num" $ do
      let ts = [(ref "a", singleton (T Num) :: TypeEnv SampleTyp Char)]
      let expr = Expression {args = [], application = (ref "a" :| [])}
      inferExpression ts expr `shouldBe` (Right (singleton (T Num)))
    it "(Num -> Num) | a" $ do
      let ts =
            [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
            , (ref "a", singleton Nil)
            ]
      let expr = Expression {args = [], application = (ref "f" :| [ref "a"])}
      inferExpression ts expr `shouldBe` (Right (singleton (T Num)))
    it "a => (Num -> Num) | a" $ do
      let ts =
            [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
            , (arg "a", singleton Nil)
            ]
      let expr =
            Expression {args = [Arg "a"], application = (ref "f" :| [arg "a"])}
      inferExpression ts expr `shouldBe`
        (Right (TypeEnv [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, T Num)]))
    it "f => f | Num" $ do
      let ts = [(arg "f", singleton Nil), (ref "a", singleton (T Num))]
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
