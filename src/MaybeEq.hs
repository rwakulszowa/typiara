module MaybeEq where

-- Wrapper around `Maybe` that considers two `Nothing` values different.
-- Falls back to regular `==` implementation in all other cases.
newtype MaybeEq a =
  MaybeEq (Maybe a)
  deriving (Show, Ord)

instance Eq a => Eq (MaybeEq a) where
  (==) (MaybeEq Nothing) (MaybeEq Nothing) = False
  (==) (MaybeEq x) (MaybeEq y) = x == y
