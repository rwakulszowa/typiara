module Typiara.Infer.Expression
  ( Ref(..)
  , ref
  , Arg(..)
  , arg
  , Expression(..)
  , inferExpression
  , InferExpressionError(..)
  ) where

import Data.Bifunctor (first)
import Data.Foldable (foldrM, toList)
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Typiara.Infer.Application (Application, decompose, inferApplication)
import Typiara.Infer.FT (FT(..))
import Typiara.Infer.Typ (Typ)
import Typiara.Infer.TypeEnv
  ( RootOrNotRoot(..)
  , TypeEnv(..)
  , UnifyEnvError(..)
  , funT
  , singleton
  , unifyEnv
  )
import Typiara.LeftOrRight (LeftOrRight)

import qualified Typiara.Utils as Utils

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
-- Note, that `y` is never used in the application.
data Expression =
  Expression
    { args :: [Arg]
    -- ^ Left hand side of a function.
    -- May be empty.
    , application :: Application (Either Arg Ref)
    -- ^ Right hand side of a function.
    -- To avoid shadowing, local arguments and external references are
    -- represented with separate types.
    }
  deriving (Eq, Show, Ord)

data InferExpressionError t v
  = MissingTypes (NonEmpty.NonEmpty (Either Arg Ref))
  | UnifyEnvError (UnifyEnvError t v)
  | RebuildError (UnifyEnvError t v)
  deriving (Eq, Show)

inferExpression ::
     (Enum v, Ord v, Show v, Show (t v), Typ t, Functor t, Foldable t)
  => Map.Map (Either Arg Ref) (TypeEnv t v)
  -> Expression
  -> Either (InferExpressionError t v) (TypeEnv t v)
inferExpression types (Expression args application) = do
  getRefT <-
    first
      MissingTypes
      (Utils.buildLookupF types (Set.fromList . toList $ application))
  (retT, inferredRefTs) <-
    first UnifyEnvError (inferRefApplication getRefT application)
  let getInferredRefT k =
        Maybe.fromMaybe (singleton Nil) (inferredRefTs Map.!? Left k)
  first RebuildError (rebuildExpr (getInferredRefT <$> args) retT)

inferRefApplication getRefT app@(funRef :| argRefs) = do
  funT <- inferApplication (getRefT <$> app)
  let (retT, argTs) = decompose funT argRefs
    -- ^ Get a type for every ref.
    -- If the same ref is used many times, it will appear multiple times.
  unifiedRefTs <-
    Utils.mapFromListWithKeyM (\_ -> unifyEnv Root) ((funRef, funT) : argTs)
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
    merge' v t baseT = unifyEnv (NotRoot v) baseT t
