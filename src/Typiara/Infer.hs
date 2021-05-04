{-# LANGUAGE FlexibleContexts #-}

module Typiara.Infer
  ( Ref(..)
  , ref
  , Arg(..)
  , arg
  , Expression(..)
  , inferExpression
  , InferExpressionError(..)
  ) where

import           Data.Bifunctor      (first)
import           Data.Data           (Data)
import           Data.Foldable       (foldlM, foldrM, toList)
import           Data.List.NonEmpty  (NonEmpty ((:|)))

import qualified Data.List.NonEmpty  as NonEmpty
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe          as Maybe
import qualified Data.Set            as Set

import           Typiara.Data.Tagged (Tagged)
import           Typiara.FT          (FT (..))
import qualified Typiara.Typ         as T
import           Typiara.TypDef      (TypDef)
import           Typiara.TypeEnv     (TypeEnv (..), UnifyEnvError (..),
                                      buildFunEnv, clean, funT, nthArgId,
                                      popArg, singleton, unifyEnv, unifyEnvR)

import qualified Typiara.Utils       as Utils

newtype Ref =
  Ref String
  deriving (Eq, Show, Ord)

newtype Arg =
  Arg String
  deriving (Eq, Show, Ord)

arg = Left . Arg

ref = Right . Ref

-- | Expression is what the user provides.
-- The result will have an arity of application + size of args.
--
-- In `x y -> Inc x`, `x y` are args, `Inc x` is the application.
data Expression =
  Expression
    { args        :: [Arg]
    -- ^ Left hand side of a function.
    -- May be empty.
    , application :: Application (Either Arg Ref)
    -- ^ Right hand side of a function.
    -- To avoid shadowing, local arguments and external references are
    -- represented with separate types.
    }
  deriving (Eq, Show, Ord)

data InferExpressionError
  = MissingTypes (NonEmpty.NonEmpty (Either Arg Ref))
  | UnifyEnvError UnifyEnvError
  | RebuildError UnifyEnvError
  | TypErr T.TypError
  deriving (Eq, Show)

-- | Infer return type of a single expression.
-- Types are provided in the user friendly form of `Typ`.
-- Internal calculations are performed on a more efficient `TypeEnv`.
-- When processing a large expression, it is more efficient to build a large `Expression`
-- and infer once, rather than inferring step by step.
inferExpression ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => Map.Map (Either Arg Ref) (T.Typ t)
  -> Expression
  -> Either InferExpressionError (T.Typ t)
inferExpression types (Expression args application) = do
  getRefT <-
    first MissingTypes (Utils.buildLookupF typeEnvs (dedup application))
  (retT, inferredRefTs) <-
    first UnifyEnvError (inferRefApplication getRefT application)
  let getInferredRefT k =
        Maybe.fromMaybe (singleton Nil) (inferredRefTs Map.!? Left k)
  retEnv <- first RebuildError (rebuildExpr (getInferredRefT <$> args) retT)
  first TypErr (T.fromTypeEnv retEnv)
  where
    typeEnvs = T.intoTypeEnv <$> types
    dedup = Set.fromList . toList

inferRefApplication getRefT app@(funRef :| argRefs) = do
  funT <- inferApplication (getRefT <$> app)
  let (retT, argTs) = decompose funT argRefs
    -- Get a type for every ref.
    -- If the same ref is used many times, it will appear multiple times.
  unifiedRefTs <-
    Utils.mapFromListWithKeyM (const unifyEnvR) ((funRef, funT) : argTs)
  return (retT, unifiedRefTs)

-- | Given a return type and types of arguments, rebuild a function args -> ret.
-- The result will have arity of at least `length args`.
-- `rebuildExpr [Str, Num] Num` == `Str -> Num -> Num`.
rebuildExpr args ret = foldrM f ret args
  where
    f arg ret =
      pure (funT (argV, Nil) (retV, Nil)) >>= merge' argV arg >>=
      merge' retV ret
    argV = toEnum 0
    retV = succ argV
    merge' v t baseT = unifyEnv v baseT t

--
-- | Application inference.
--
-- | Flat application.
-- The type has no knowledge about the underlying type. It can only traverse
-- the items from left to right.
-- By convention, head is the function and tail contains arguments.
type Application a = NonEmpty.NonEmpty a

-- | Merge things together, returning a type deduced from applying arguments
-- to a function.
-- The caller is responsible for mapping the result back to some external
-- references. The returned type contains all information regarding deduced
-- types of each application element.
--
-- NOTE: the function only adds constraints deduced from application shape.
-- It *does not* reduce the input function's arity in any way.
-- In other words, this is more of an implementation details of `Expression`.
-- See `inferExpression` for a more user friendly interface.
inferApplication ::
     (TypDef t, Foldable t, Functor t, Tagged t, Eq (t Int))
  => Application (TypeEnv t)
  -> Either UnifyEnvError (TypeEnv t)
inferApplication (x NonEmpty.:| []) = Right x
inferApplication (fun :| args) = do
  let arity = length args
  appEnv <- unifyEnvR fun (buildFunEnv arity)
  -- Application shape allows us to deduce required arity for the function.
  let cleanEnv = clean appEnv
  (result, _) <-
    foldlM
      (\(t, n) arg -> do
         t' <- handleNthApplication t n arg
         return (t', n + 1))
      (cleanEnv, 0)
      args
  return (clean result)
  where
    handleNthApplication accT n argT = unifyEnv (nthArgId accT n) accT argT

-- | Decompose a type into individual application tokens.
-- Each arg is bound to n-th argument, while `ret` is the bit remaining after
-- applying all arguments.
-- The applied function is not explicitly returned - it is already available
-- to the caller as `typ`.
decompose :: (Foldable t) => TypeEnv t -> [b] -> (TypeEnv t, [(b, TypeEnv t)])
decompose typ [] = (typ, [])
decompose typ (x:xs) =
  let (argT, retT) = popArg typ
      (recRetT, recBindings) = decompose retT xs
   in (recRetT, (x, argT) : recBindings)
