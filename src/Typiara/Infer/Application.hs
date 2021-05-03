{-# LANGUAGE FlexibleContexts #-}

module Typiara.Infer.Application
  ( Application
  , inferApplication
  , decompose
  ) where

import           Data.Data           (Data)
import           Data.Foldable       (foldlM)
import           Data.List.NonEmpty  (NonEmpty ((:|)))
import qualified Data.List.NonEmpty  as NonEmpty

import           Typiara.Data.Tagged (Tagged)
import           Typiara.Typ         (Typ)
import           Typiara.TypeEnv     (TypeEnv (root), UnifyEnvError,
                                      buildFunEnv, clean, nthArgId, popArg,
                                      unifyEnv, unifyEnvR)

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
     (Typ t, Foldable t, Functor t, Tagged t, Eq (t Int))
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
