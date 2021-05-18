{-# LANGUAGE FlexibleContexts #-}

-- | Core functionality exported in a user friendly format.
module Typiara
  ( apply
  , applyAt
  , ApplyAtError
  , merge
  , TypDef(..)
  , FT(..)
  , Typ(..)
  , UnifyResult(..)
  , UnifyError(..)
  , Tagged(..)
  , fromEnumTree
  , singleton
  ) where

import           Data.Bifunctor      (first)
import           Data.Data           (Data)
import           Data.Either         (fromLeft)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Map.Strict     (fromList)
import           Typiara.Data.Tagged (Tagged (..))
import           Typiara.FT          (FT (..))
import           Typiara.Infer       (Expression (..), InferExpressionError,
                                      arg, inferExpression, ref)
import           Typiara.Typ         (Typ, arity, fromEnumTree, fun, makeFun,
                                      merge, singleton)
import           Typiara.TypDef      (TypDef (..), UnifyError (..),
                                      UnifyResult (..))

-- | Apply args to a function in a single bulk operation.
apply ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => Typ t
  -> [Typ t]
  -> Either InferExpressionError (Typ t)
apply f xs =
  inferExpression (fromList ((f', f) : (xs' `zip` xs))) (applicationExpr f' xs')
  where
    n = length xs
    f' = ref "f"
    xs' = (\i -> ref ("x" ++ show i)) <$> [0 .. n - 1]
    applicationExpr f xs = Expression {args = [], application = f :| xs}

-- | Apply an argument at a specified index.
-- Builds a hoistping higher order function and apples it to the argument.
--
-- In short, `applyAt f x 2 == \a b -> apply f [a b x]`, except with links preserved
-- (see a note in Infer.hs for a note on links preservation).
applyAt ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int), Show (t Int))
  => Typ t
  -> Typ t
  -> Int
  -> Either ApplyAtError (Typ t)
applyAt f x i
  | i >= arity f = Left (BadArity i (arity f))
applyAt f x i = first ApplyErr $ apply hoistF [f, x] -- Merged into a single `apply` call for performance.
    -- hoist the i'th argument to the front.
    -- Arguments [0 .. i] are shifted to the right.
    -- Other arguments are left intact.
  where
    hoistF = makeFun [args, hoistedArgs]
    argsN = i + 1
    args = [0 .. argsN]
    hoistedArgs = i : ([0 .. i - 1] <> [i + 1 .. argsN])
    unwrap (Right x) = x

data ApplyAtError
  = ApplyErr InferExpressionError
  | BadArity Int Int
  deriving (Eq, Show)
