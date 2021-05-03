{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infer.ExpressionSpec
  ( spec
  ) where

import           Data.List.NonEmpty        (NonEmpty (..))
import           Test.Hspec

import           Typiara.FT
import           Typiara.Infer.Application
import           Typiara.Infer.Expression
import           Typiara.SampleTyp
import           Typiara.Typ
import           Typiara.TypeEnv

-- | Fixed type variant to avoid ambiguous inference failures.
singleton' :: FT SampleTyp Int -> TypeEnv SampleTyp Int
singleton' = singleton

spec :: Spec
spec = do
  describe "inferExpression" $ do
    describe "basic" $ do
      it "Num" $ do
        let ts = [(ref "a", singleton' (T Num))]
        let expr = Expression {args = [], application = ref "a" :| []}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
    describe "fafa" $ do
      it "(Num -> Num) | a" $ do
        let ts =
              [ (ref "f", TypeEnv {tvs = [(0, F 1 1), (1, T Num)], root = 0})
              , (ref "a", singleton' Nil)
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
      it "a => (Num -> Num) | a" $ do
        let ts =
              [ (ref "f", TypeEnv {tvs = [(0, F 1 1), (1, T Num)], root = 0})
              , (arg "a", singleton' Nil)
              ]
        let expr =
              Expression {args = [Arg "a"], application = ref "f" :| [arg "a"]}
        inferExpression ts expr `shouldBe`
          Right (TypeEnv {tvs = [(0, F 1 2), (1, T Num), (2, T Num)], root = 0})
      it "f => f | Num" $ do
        let ts = [(arg "f", singleton' Nil), (ref "a", singleton' (T Num))]
        let expr =
              Expression {args = [Arg "f"], application = arg "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Right
            (TypeEnv [(0, F 1 2), (1, F 3 4), (2, Nil), (3, T Num), (4, Nil)] 0)
      it "f a => f | a" $ do
        let ts = [(arg "f", singleton' Nil), (arg "a", singleton' Nil)]
        let expr =
              Expression
                {args = [Arg "f", Arg "a"], application = arg "f" :| [arg "a"]}
        inferExpression ts expr `shouldBe`
          Right
            (TypeEnv
               [ (0, F 1 2)
               , (1, F 3 4)
               , (2, F 6 7)
               , (3, Nil)
                -- 5 missing to match the exact return value.
                -- TODO: custom `Eq` implementation, ignoring id values.
               , (4, Nil)
               , (6, Nil)
               , (7, Nil)
               ]
               0)
    describe "containers" $ do
      it "(Seq a -> a) | (Seq Num)" $ do
        let ts =
              [ ( ref "f"
                , TypeEnv
                    {tvs = [(0, F 1 2), (1, T (Seq 2)), (2, Nil)], root = 0})
              , ( ref "a"
                , TypeEnv {tvs = [(0, T (Seq 1)), (1, T Num)], root = 0})
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
      it "((a -> b) -> (b -> c) -> (a -> c)) | (Num -> Bool) | (Bool -> Str)" $
        -- TODO: clean up manual `toEnum` calls below after implementing custom `Eq`.
       do
        let ts =
              [ ( ref "f"
                , TypeEnv
                    [ ('R', F 'F' 'G')
                    , ('F', F 'a' 'b')
                    , ('G', F 'H' 'I')
                    , ('H', F 'b' 'c')
                    , ('I', F 'a' 'c')
                    , ('a', Nil)
                    , ('b', Nil)
                    , ('c', Nil)
                    ]
                    'R')
              , ( ref "g"
                , TypeEnv [('R', F 'x' 'y'), ('x', T Num), ('y', T Bool)] 'R')
              , ( ref "h"
                , TypeEnv [('R', F 'x' 'y'), ('x', T Bool), ('y', T Str)] 'R')
              ]
        let expr =
              Expression
                {args = [], application = ref "f" :| [ref "g", ref "h"]}
        inferExpression ts expr `shouldBe`
          Right
            (TypeEnv
               [ (toEnum 0, F (toEnum 5) (toEnum 7))
               , (toEnum 5, T Num)
               , (toEnum 7, T Str)
               ]
               (toEnum 0))
    describe "cycles" $
      -- TODO: how does one actually introduce cycles just by applying functions?
     do return ()
    describe "negative" $ do
      it "(Num -> Num) | Str" $ do
        let ts =
              [(ref "f", funT' (T Num) (T Num)), (ref "a", singleton' (T Str))]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Left (UnifyEnvError (UnifyError (ConflictingTypes "Num" "Str")))
      it "Num | Num" $ do
        let ts = [(ref "f", singleton' (T Num)), (ref "a", singleton' (T Num))]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Left (UnifyEnvError (UnifyError (ConflictingTypes "T.Num" "F")))
