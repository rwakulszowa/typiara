{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable,
  FlexibleInstances #-}

module Typiara.Infer.FT (FT(..)) where

-- | Function or not-a-function (or an empty type).
-- Functions are special. All other types are provided by the user, but
-- functions require extra care.
--
-- `Nil` is used when no constraints are known.
data FT t v
  = F v v
  | T (t v)
  | Nil
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable)
