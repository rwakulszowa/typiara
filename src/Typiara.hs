{-# LANGUAGE FlexibleContexts #-}

-- | Core functionality exported in a user friendly format.
module Typiara
  ( apply
  , applyAt
  , reorder
  , reorderT
  , ApplyAtError
  , merge
  , mergeNth
  , popArg
  , popArgs
  , TypDef(..)
  , FT(..)
  , Typ(..)
  , UnifyResult(..)
  , UnifyError(..)
  , ApplyError(..)
  , Tagged(..)
  , fromEnumTree
  , singleton
  ) where

import           Data.Bifunctor      (first)
import           Data.Data           (Data)
import           Data.Either         (fromLeft)
import           Data.List           (sort)
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Map.Strict     (fromList)
import           Data.Maybe          (fromJust)
import           Typiara.Data.Tagged (Tagged (..))
import           Typiara.FT          (FT (..))
import           Typiara.Infer       (Expression (..),
                                      InferExpressionError (..), arg,
                                      inferExpression, ref)
import           Typiara.Typ         (Typ, TypError, arity, fromEnumTree, fun,
                                      makeFun, merge, mergeNth, popArg, popArgs,
                                      singleton)
import           Typiara.TypDef      (TypDef (..), UnifyError (..),
                                      UnifyResult (..))
import           Typiara.TypeEnv     (UnifyEnvError)

-- | A subset of expression inference errors.
-- Other possible errors are internal and are not expected to happen when using this function.
newtype ApplyError =
  ApplyError UnifyEnvError
  deriving (Eq, Show)

-- | Apply args to a function in a single bulk operation.
apply ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => Typ t
  -> [Typ t]
  -> Either ApplyError (Typ t)
apply f xs =
  first
    mapErr
    (inferExpression
       (fromList ((f', f) : (xs' `zip` xs)))
       (applicationExpr f' xs'))
  where
    n = length xs
    f' = ref "f"
    xs' = (\i -> ref ("x" ++ show i)) <$> [0 .. n - 1]
    applicationExpr f xs = Expression {args = [], application = f :| xs}
    mapErr (UnifyEnvError e) = ApplyError e
    mapErr e = error ("Unexpected application error: " ++ show e)

-- | Apply an argument at a specified index.
-- Builds a hoistping higher order function and apples it to the argument.
--
-- In short, `applyAt f x 2 == \a b -> apply f [a b x]`, except with links preserved
-- (see a note in Infer.hs for a note on links preservation).
applyAt ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => Typ t
  -> Typ t
  -> Int
  -> Either ApplyAtError (Typ t)
applyAt f x i
  | i >= arity f = Left (BadArity i (arity f))
applyAt f x i = first ApplyErr $ apply (hoist f i) [x]
    -- hoist the i'th argument to the front.
    -- Arguments [0 .. i] are shifted to the right.
    -- Other arguments are left intact.
  where
    hoist f i =
      fromJust $ reorder f (i : ([0 .. i - 1] <> [i + 1 .. arity f - 1]))

-- | Reorder type arguments.
-- Nothing if order is not a permutation of [0 .. arity - 1].
reorder ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => Typ t
  -> [Int]
  -> Maybe (Typ t)
reorder t o = unwrap . (`apply` [t]) <$> reorderT o
  where
    unwrap (Right x) = x
    unwrap (Left e)  = error $ show ("Application failed", e)

-- | Type representing a function that, after applying a function `f` to it,
-- will produce `f` with reordered arguments.
reorderT ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => [Int]
  -> Maybe (Typ t)
reorderT o =
  if sort o == naturalOrder
    then Just reorderT
    else Nothing
  where
    a = length o
    naturalOrder = [0 .. a - 1]
          -- Add the return value's index to both signatures.
          -- The caller is not allowed to move it forward, but the `makeFun` function
          -- expects it to be present.
    reorderT = makeFun [naturalOrder <> [a], o <> [a]]

data ApplyAtError
  = ApplyErr ApplyError
  | BadArity Int Int
  deriving (Eq, Show)
