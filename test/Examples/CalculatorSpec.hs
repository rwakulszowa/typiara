{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}

-- | A simple type, that recognizes only integers and functions.
module Examples.CalculatorSpec
  ( spec
  ) where

import           Data.Data   (Data, Typeable)
import           Data.Either (isLeft)
import           Data.Tree
import           Test.Hspec
import           Typiara

leaf x = Node x []

unwrap (Right x) = x
unwrap (Left e)  = error $ show e

-- | Data type representing leaves.
-- Function type is not provided by the user - it's handled by Typiara itself.
-- See `FT.hs` for core types handled by the library.
data CalculatorT a =
  CalcInt
  deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable, Data, Typeable)

instance TypDef CalculatorT where
  unify CalcInt CalcInt = Right (UnifyResult CalcInt [])

instance Tagged CalculatorT where
  tag CalcInt = "CalcInt"
  fromTag "CalcInt" [] = Just CalcInt
  fromTag _ _          = Nothing

spec :: Spec
spec = do
  describe "Any" $ do
    let f =
          unwrap $
          fromEnumTree
            (Node 'f' [leaf 'a', leaf 'b'])
            [('f', "F"), ('a', "T.CalcInt"), ('b', "T.CalcInt")]
    let x = singleton (T CalcInt)
    it "f x" $ do (f `apply` [x]) `shouldBe` Right x
    it "f (f (f x))" $ do
      let ap fun arg = fun `apply` [arg]
      (ap f x >>= ap f >>= ap f) `shouldBe` Right x
    it "application to leaf" $ do (x `apply` [x]) `shouldSatisfy` isLeft
    it "bad arity" $ do (f `apply` [x, x]) `shouldSatisfy` isLeft
