{-# LANGUAGE FlexibleContexts #-}

-- | Core functionality exported in a user friendly format.
module Typiara
  ( apply
  , Typ(..)
  , FT(..)
  , UnifyResult(..)
  , UnifyError(..)
  , Tagged(..)
  , fromEnumTree
  , fromTree
  , singleton
  ) where

import           Data.Data                (Data)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Map.Strict          (fromList)
import           Typiara.Data.Tagged      (Tagged (..))
import           Typiara.FT               (FT (..))
import           Typiara.Infer.Expression (Expression (..),
                                           InferExpressionError,
                                           inferExpression, ref)
import           Typiara.Typ              (Typ (..), UnifyError (..),
                                           UnifyResult (..))
import           Typiara.TypeEnv          (RootOrNotRoot, TypeEnv, fromEnumTree,
                                           fromTree, singleton)

-- | Apply args to a function in a single bulk operation.
apply ::
     ( Enum v
     , Ord v
     , Typ t
     , Functor t
     , Foldable t
     , Data v
     , Tagged t (RootOrNotRoot v)
     , Eq (t (RootOrNotRoot v))
     )
  => TypeEnv t v
  -> [TypeEnv t v]
  -> Either (InferExpressionError v) (TypeEnv t v)
apply f xs =
  inferExpression (fromList ((f', f) : (xs' `zip` xs))) (applicationExpr f' xs')
  where
    n = length xs
    f' = ref "f"
    xs' = (\i -> ref ("x" ++ show i)) <$> [0 .. n - 1]
    applicationExpr f xs = Expression {args = [], application = f :| xs}
