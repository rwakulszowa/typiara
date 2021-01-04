module Typiara.MaybeEq where

import Data.Traversable (mapAccumL)

-- | Wrapper around `Maybe` that considers two `Nothing` values different.
-- Falls back to regular `==` implementation in all other cases.
--
-- NOTE: this type *must not* implement `Ord`. The `Eq` implementation below
-- does not allow ordering.
newtype MaybeEq a =
  MaybeEq (Maybe a)
  deriving Show

instance Eq a => Eq (MaybeEq a) where
  (==) (MaybeEq Nothing) (MaybeEq Nothing) = False
  (==) (MaybeEq x) (MaybeEq y) = x == y


-- | `MaybeEq Nothing` values represent unique elements that should never
-- be considered equal. However, the type doesn't impment `Ord`.
-- Replace each `Nothing` value with a unique integer, to make it usable in
-- `Ord` contexts.
-- NOTE: may silently overflow.
fillMaybeEq :: (Traversable t) => t (MaybeEq a) -> t (Either Int a)
fillMaybeEq = snd . mapAccumL fillOne 0
    where fillOne :: Int -> (MaybeEq a) -> (Int, (Either Int a))
          fillOne state (MaybeEq (Just a)) = (state, Right a)
          fillOne state (MaybeEq Nothing) = (succ state, Left state)
