{-# LANGUAGE OverloadedLists #-}

module TypiaraSpec
  ( spec
  ) where

import           Test.Hspec

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map           (Map, fromList)
import           Data.Tree          (Tree (..))

import           Typiara            (FT (..), apply, applyAt, reorder)
import           Typiara.Infer
import           Typiara.SampleTyp
import           Typiara.Typ
import           Typiara.TypeEnv    (UnifyEnvError (..))
import           Typiara.Utils      (fromRight)

import           Debug.Trace

leaf x = Node x []

te :: Tree Char -> Map Char String -> Typ SampleTyp
te a = fromRight . fromEnumTree a

spec :: Spec
spec =
  describe "inference" $
    -- Int
   do
    let int = te (leaf 'a') [('a', "T.Num")]
    -- [Int]
    let seq = te (Node 's' [leaf 'a']) [('s', "T.Seq"), ('a', "T.Num")]
    -- Int -> Int
    let inc = te (Node 'f' [leaf 'a', leaf 'a']) [('f', "F"), ('a', "T.Num")]
    -- [a] -> a
    let head =
          te
            (Node 'f' [Node 's' [leaf 'a'], leaf 'a'])
            [('f', "F"), ('s', "T.Seq"), ('a', "Nil")]
    -- a -> a
    let id = te (Node 'f' [leaf 'a', leaf 'a']) [('f', "F"), ('a', "Nil")]
    -- a -> [a]
    let cons =
          te
            (Node 'f' [leaf 'a', Node 's' [leaf 'a']])
            [('f', "F"), ('s', "T.Seq"), ('a', "Nil")]
    -- (b -> c) -> (a -> b) -> (a -> c)
    let compose =
          te
            (Node
               'f'
               [ Node 'g' [leaf 'b', leaf 'c']
               , Node
                   'h'
                   [ Node 'i' [leaf 'a', leaf 'b']
                   , Node 'j' [leaf 'a', leaf 'c']
                   ]
               ])
            [ ('f', "F")
            , ('g', "F")
            , ('h', "F")
            , ('i', "F")
            , ('j', "F")
            , ('a', "Nil")
            , ('b', "Nil")
            , ('c', "Nil")
            ]
    -- (a -> a) -> a
    let fix =
          te
            (Node 'f' [Node 'g' [leaf 'a', leaf 'a'], leaf 'a'])
            [('f', "F"), ('g', "F"), ('a', "Nil")]
    describe "apply" $ do
      it "id . id" $
          -- Validate that links are not lost in the process.
          -- `Nil a, Nil b => a -> b` is very different from `Nil a => a -> a`.
       do (compose `apply` [id, id]) `shouldBe` Right id
      it "inc . head" $
        (compose `apply` [inc, head]) `shouldBe`
        Right
          (te
             (Node 'f' [Node 's' [leaf 'a'], leaf 'a'])
             [('f', "F"), ('s', "T.Seq"), ('a', "T.Num")])
      it "(inc . head) $ seq" $
        (compose `apply` [inc, head, seq]) `shouldBe` Right int
      it "(cons . inc) $ int" $
        (compose `apply` [cons, inc, int]) `shouldBe` Right seq
      it "fix $ inc" $ (fix `apply` [inc]) `shouldBe` Right int
      it "fix $ head" $
        (fix `apply` [head]) `shouldBe`
        (Left . UnifyEnvError . Cycle) ["T.Seq", "T.Seq", "F", "F"]
      it "cons . cons $ seq" $
        -- Validate nested generics inference.
        (compose `apply` [cons, cons, int]) `shouldBe`
        Right
          (te
             (Node 's' [Node 't' [leaf 'a']])
             [('s', "T.Seq"), ('t', "T.Seq"), ('a', "T.Num")])
    describe "applyAt" $ do
      let applyAt' x i f = applyAt f x i
      it "inc . head, regular application order" $
        (pure compose >>= applyAt' inc 0 >>= applyAt' head 0) `shouldBe`
        (Right . fromRight $ compose `apply` [inc, head])
      it "inc . head, reverse application order" $
        (pure compose >>= applyAt' head 1 >>= applyAt' inc 0) `shouldBe`
        (Right . fromRight $ compose `apply` [inc, head])
      it "Bool -> Num -> Str -> Nil" $ do
        let t =
              te
                (Node
                   'f'
                   [ leaf 'a'
                   , Node 'g' [leaf 'b', Node 'h' [leaf 'c', leaf 'd']]
                   ])
                [ ('f', "F")
                , ('g', "F")
                , ('h', "F")
                , ('a', "T.Bool")
                , ('b', "T.Num")
                , ('c', "T.Str")
                , ('d', "Nil")
                ]
        let s = te (leaf 'a') [('a', "T.Str")]
        let b = te (leaf 'a') [('a', "T.Bool")]
        (pure t >>= applyAt' s 2 >>= applyAt' int 1 >>= applyAt' b 0) `shouldBe`
          Right (singleton Nil)
    describe "reorder" $ do
      it "inc" $ reorder inc [0] `shouldBe` Just inc
      it "const3" $ do
        let t = (makeFun [[0, 1, 2, 0]] :: Typ SampleTyp)
        reorder t [1, 2, 0] `shouldBe` Just (makeFun [[1, 2, 0, 0]])
    describe "corner cases" $ do
      it "self merge" $
        -- The function will attempt to merge c with d, where d is already
        -- a parent of c.
        -- Forms a cycle on a single variable.
        -- Regression test.
       do
        let x =
              te
                (Node
                   'a'
                   [ Node 'b' [leaf 'c', Node 'd' [leaf 'c', leaf 'c']]
                   , leaf 'e'
                   ])
                [('a', "F"), ('b', "F"), ('c', "Nil"), ('d', "F"), ('e', "Nil")]
        let y = te (Node 'a' [leaf 'b', leaf 'b']) [('a', "F"), ('b', "Nil")]
        (x `apply` [y]) `shouldBe`
          (Left . UnifyEnvError . Cycle) ["F", "F", "F", "F"]
