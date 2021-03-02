{-# LANGUAGE OverloadedLists #-}

module LibSpec
  ( spec
  ) where

import Test.Hspec

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map, fromList)
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
    -- | Apply args to a function in a single bulk operation.
    -- There's information stored in variable names - we have to apply all
    -- args at once (i.e. in a single `inferExpression` call), or else the
    -- information stored in variable names won't propagate.
    let apply' f xs =
          inferExpression
            (fromList ((f', f) : (xs' `zip` xs)))
            (applicationExpr f' xs')
          where
            n = length xs
            f' = ref "f"
            xs' = (\i -> ref ("x" ++ show i)) <$> [0 .. n - 1]
            applicationExpr f xs = Expression {args = [], application = f :| xs}
    describe "apply" $ do
      it "id . id" $
          -- Validate that links are not lost in the process.
          -- `Nil a, Nil b => a -> b` is very different from `Nil a => a -> a`.
       do (compose `apply'` [id, id]) `shouldBe` Right id
      it "inc . head" $
        (compose `apply'` [inc, head]) `shouldBe`
        Right
          (te
             (Node 'f' [Node 's' [leaf 'a'], leaf 'a'])
             [('f', "F"), ('s', "T.Seq"), ('a', "T.Num")])
      it "(inc . head) $ seq" $
        (compose `apply'` [inc, head, seq]) `shouldBe` Right int
      it "(cons . inc) $ int" $
        (compose `apply'` [cons, inc, int]) `shouldBe` Right seq
