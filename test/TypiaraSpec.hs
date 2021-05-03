{-# LANGUAGE OverloadedLists #-}

module TypiaraSpec
  ( spec
  ) where

import           Test.Hspec

import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Map                 (Map, fromList)
import           Data.Tree                (Tree (..))

import           Typiara                  (apply)
import           Typiara.Infer.Expression
import           Typiara.SampleTyp
import           Typiara.TypeEnv
import           Typiara.Utils            (fromRight)

import           Debug.Trace

leaf x = Node x []

te :: Tree Char -> Map Char String -> TypeEnv SampleTyp
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
