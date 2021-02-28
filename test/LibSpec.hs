{-# LANGUAGE OverloadedLists #-}

module LibSpec
  ( spec
  ) where

import Test.Hspec

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Tree (Tree(..))

import Typiara.Infer.Expression
import Typiara.SampleTyp
import Typiara.TypeEnv
import Typiara.Utils (fromRight)

leaf x = Node x []

te :: Tree Char -> Map Char String -> TypeEnv SampleTyp Char
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
    let apply' x f = inferExpression [(f', f), (x', x)] (applicationExpr f' x')
          where
            f' = ref "f"
            x' = ref "x"
            applicationExpr f x = Expression {args = [], application = f :| [x]}
    describe "apply" $ do
      xit "inc . head" $
        (pure compose >>= apply' inc >>= apply' head) `shouldBe`
        Right
          (te
             (Node 'f' [Node 's' [leaf 'a'], leaf 'a'])
             [('f', "F"), ('s', "T.Seq"), ('a', "T.Num")])
      it "(inc . head) $ seq" $
        (pure compose >>= apply' inc >>= apply' head >>= apply' seq) `shouldBe`
        Right int
      xit "(cons . inc) $ int" $
        (pure compose >>= apply' cons >>= apply' inc >>= apply' int) `shouldBe`
        Right seq
