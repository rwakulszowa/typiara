module Typiara.UniqueItemSource
  ( UniqueItemSource
  , fromList
  , NextErr(..)
  , next
  , takeUnique
  ) where

import qualified Data.Set as Set

import Data.Set (Set)

-- TODO: add an unsafe, unchecked variant once performance becomes an issue.
data UniqueItemSource a =
  CheckedUniqueItemSource
    { source :: [a]
    , seen :: Set a
    }
  deriving (Show, Eq)

fromList :: (Ord a) => [a] -> UniqueItemSource a
fromList source = CheckedUniqueItemSource source Set.empty

data NextErr a
  = SourceDepleted
  | Duplicate a
  deriving (Eq, Show)

next ::
     (Ord a) => UniqueItemSource a -> Either (NextErr a) (a, UniqueItemSource a)
next (CheckedUniqueItemSource [] _) = Left SourceDepleted
next (CheckedUniqueItemSource (a:as) seen)
  | a `Set.member` seen = Left $ Duplicate a
  | otherwise = Right (a, CheckedUniqueItemSource as (a `Set.insert` seen))

takeUnique :: (Ord a) => Int -> UniqueItemSource a -> Either (NextErr a) [a]
takeUnique 0 _ = Right []
takeUnique n source = do
  (x, source) <- next source
  tail <- takeUnique (n - 1) source
  return $ x : tail
