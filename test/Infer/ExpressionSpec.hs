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
              [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
              , (ref "a", singleton' Nil)
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
      it "a => (Num -> Num) | a" $ do
        let ts =
              [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
              , (arg "a", singleton' Nil)
              ]
        let expr =
              Expression {args = [Arg "a"], application = ref "f" :| [arg "a"]}
        inferExpression ts expr `shouldBe`
          Right
            (TypeEnv [(Root, F 0 1), (NotRoot 0, T Num), (NotRoot 1, T Num)])
      it "f => f | Num" $ do
        let ts = [(arg "f", singleton' Nil), (ref "a", singleton' (T Num))]
        let expr =
              Expression {args = [Arg "f"], application = arg "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Right
            (TypeEnv
               [ (Root, F 0 1)
               , (NotRoot 0, F 2 3)
               , (NotRoot 1, Nil)
               , (NotRoot 2, T Num)
               , (NotRoot 3, Nil)
               ])
      it "f a => f | a" $ do
        let ts = [(arg "f", singleton' Nil), (arg "a", singleton' Nil)]
        let expr =
              Expression
                {args = [Arg "f", Arg "a"], application = arg "f" :| [arg "a"]}
        inferExpression ts expr `shouldBe`
          Right
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
               ])
    describe "containers" $ do
      it "(Seq a -> a) | (Seq Num)" $ do
        let ts =
              [ ( ref "f"
                , TypeEnv
                    [(Root, F 0 1), (NotRoot 0, T (Seq 1)), (NotRoot 1, Nil)])
              , (ref "a", TypeEnv [(Root, T (Seq 0)), (NotRoot 0, T Num)])
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
      it "((a -> b) -> (b -> c) -> (a -> c)) | (Num -> Bool) | (Bool -> Str)" $
        -- TODO: clean up manual `toEnum` calls below after implementing custom `Eq`.
       do
        let ts =
              [ ( ref "f"
                , TypeEnv
                    [ (Root, F 'F' 'G')
                    , (NotRoot 'F', F 'a' 'b')
                    , (NotRoot 'G', F 'H' 'I')
                    , (NotRoot 'H', F 'b' 'c')
                    , (NotRoot 'I', F 'a' 'c')
                    , (NotRoot 'a', Nil)
                    , (NotRoot 'b', Nil)
                    , (NotRoot 'c', Nil)
                    ])
              , ( ref "g"
                , TypeEnv
                    [ (Root, F 'x' 'y')
                    , (NotRoot 'x', T Num)
                    , (NotRoot 'y', T Bool)
                    ])
              , ( ref "h"
                , TypeEnv
                    [ (Root, F 'x' 'y')
                    , (NotRoot 'x', T Bool)
                    , (NotRoot 'y', T Str)
                    ])
              ]
        let expr =
              Expression
                {args = [], application = ref "f" :| [ref "g", ref "h"]}
        inferExpression ts expr `shouldBe`
          Right
            (TypeEnv
               [ (Root, F (toEnum 4) (toEnum 6))
               , (NotRoot (toEnum 4), T Num)
               , (NotRoot (toEnum 6), T Str)
               ])
    describe "cycles" $
      -- TODO: how does one actually introduce cycles just by applying functions?
     do return ()
    describe "negative" $ do
      it "(Num -> Num) | Str" $ do
        let ts =
              [ (ref "f", TypeEnv [(Root, F 0 0), (NotRoot 0, T Num)])
              , (ref "a", singleton' (T Str))
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Left (UnifyEnvError (UnifyError (ConflictingTypes "T.Num" "T.Str")))
      it "Num | Num" $ do
        let ts = [(ref "f", singleton' (T Num)), (ref "a", singleton' (T Num))]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Left (UnifyEnvError (UnifyError (ConflictingTypes "T.Num" "F")))
