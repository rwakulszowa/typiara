{-# LANGUAGE FlexibleContexts #-}

-- | Core functionality exported in a user friendly format.
module Typiara
  ( apply
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

import           Data.Data           (Data)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Map.Strict     (fromList)
import           Typiara.Data.Tagged (Tagged (..))
import           Typiara.FT          (FT (..))
import           Typiara.Infer       (Expression (..), InferExpressionError,
                                      inferExpression, ref)
import           Typiara.Typ         (Typ, fromEnumTree, merge, singleton)
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
