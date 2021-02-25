{-# LANGUAGE DeriveTraversable, FlexibleInstances,
  MultiParamTypeClasses #-}

module Typiara.FT
  ( FT(..)
  ) where

import Typiara.Data.Tagged (Tagged(..))

-- | Function or not-a-function (or an empty type).
-- Functions are special. All other types are provided by the user, but
-- functions require extra care.
--
-- `Nil` is used when no constraints are known.
data FT t v
  = F v v
  | T (t v)
  | Nil
  deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable)

instance (Tagged t v) => Tagged (FT t) v where
  tag (F _ _) = "F"
  tag (T t) = "T." ++ tag t
  tag Nil = "Nil"
  fromTag "Nil" [] = Just Nil
  fromTag "F" [a, b] = Just (F a b)
  fromTag ('T':'.':ts) xs = T <$> fromTag ts xs
