{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InferSpec
  ( spec
  ) where

import qualified Data.IntMap.Strict as IM
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict    as M
import           Test.Hspec
import           Typiara.FT
import           Typiara.Infer
import           Typiara.SampleTyp
import qualified Typiara.Typ        as T
import           Typiara.TypDef
import           Typiara.TypeEnv

-- | Fixed type variant to avoid ambiguous inference failures.
singleton' :: FT SampleTyp Int -> T.Typ SampleTyp
singleton' = T.singleton

typ' :: [(Int, FT SampleTyp Int)] -> T.Typ SampleTyp
typ' tvs = unwrap (T.typ root tvs')
  where
    unwrap (Right x) = x
    root = fst . head $ tvs
    tvs' = IM.fromList tvs

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
              [ (ref "f", typ' [(0, F 1 1), (1, T Num)])
              , (ref "a", singleton' Nil)
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
      it "a => (Num -> Num) | a" $ do
        let ts =
              [ (ref "f", typ' [(0, F 1 1), (1, T Num)])
              , (arg "a", singleton' Nil)
              ]
        let expr =
              Expression {args = [Arg "a"], application = ref "f" :| [arg "a"]}
        inferExpression ts expr `shouldBe`
          Right (typ' [(0, F 1 2), (1, T Num), (2, T Num)])
      it "f => f | Num" $ do
        let ts = [(arg "f", singleton' Nil), (ref "a", singleton' (T Num))]
        let expr =
              Expression {args = [Arg "f"], application = arg "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Right (typ' [(0, F 1 2), (1, F 3 4), (2, Nil), (3, T Num), (4, Nil)])
      it "f a => f | a" $ do
        let ts = [(arg "f", singleton' Nil), (arg "a", singleton' Nil)]
        let expr =
              Expression
                {args = [Arg "f", Arg "a"], application = arg "f" :| [arg "a"]}
        inferExpression ts expr `shouldBe`
          Right
            (typ'
               [ (0, F 1 2)
               , (1, F 3 4)
               , (2, F 6 7)
               , (3, Nil)
               , (4, Nil)
               , (6, Nil)
               , (7, Nil)
               ])
    describe "containers" $ do
      it "(Seq a -> a) | (Seq Num)" $ do
        let ts =
              [ (ref "f", typ' [(0, F 1 2), (1, T (Seq 2)), (2, Nil)])
              , (ref "a", typ' [(0, T (Seq 1)), (1, T Num)])
              ]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe` Right (singleton' (T Num))
      it "((a -> b) -> (b -> c) -> (a -> c)) | (Num -> Bool) | (Bool -> Str)" $ do
        let ts =
              [ ( ref "f"
                , typ'
                    [ (99, F 10 11)
                    , (10, F 0 1)
                    , (11, F 12 13)
                    , (12, F 1 2)
                    , (13, F 0 2)
                    , (0, Nil)
                    , (1, Nil)
                    , (2, Nil)
                    ])
              , (ref "g", typ' [(2, F 0 1), (0, T Num), (1, T Bool)])
              , (ref "h", typ' [(2, F 0 1), (0, T Bool), (1, T Str)])
              ]
        let expr =
              Expression
                {args = [], application = ref "f" :| [ref "g", ref "h"]}
        inferExpression ts expr `shouldBe`
          Right
            (typ'
               [ (toEnum 0, F (toEnum 5) (toEnum 7))
               , (toEnum 5, T Num)
               , (toEnum 7, T Str)
               ])
    describe "cycles" $
      -- TODO: how does one actually introduce cycles just by applying functions?
     do return ()
    describe "negative" $ do
      it "(Num -> Num) | Str" $ do
        let ts =
              [(ref "f", T.fun' (T Num) (T Num)), (ref "a", singleton' (T Str))]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Left (UnifyEnvError (UnifyError (ConflictingTypes "Num" "Str")))
      it "Num | Num" $ do
        let ts = [(ref "f", singleton' (T Num)), (ref "a", singleton' (T Num))]
        let expr = Expression {args = [], application = ref "f" :| [ref "a"]}
        inferExpression ts expr `shouldBe`
          Left (UnifyEnvError (UnifyError (ConflictingTypes "T.Num" "F")))
