module Typiara.Data.UniqueItemSource
  ( UniqueItemSource
  , fromList
  , NextErr(..)
  , next
  , takeUnique
  , zipUnique
  ) where

import qualified Data.Set as Set

import           Data.Set (Set)

-- TODO: add an unsafe, unchecked variant once performance becomes an issue.
data UniqueItemSource a =
  CheckedUniqueItemSource
    { source :: [a]
    , seen   :: Set a
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

-- Doesn't handle infinite lists too well.
-- The computation cannot (AFAIK) be done lazily, because the wrapping Either has to be decided eagerly.
zipUnique :: (Ord b) => [a] -> UniqueItemSource b -> Either (NextErr b) [(a, b)]
zipUnique xs uis =
  let n = length xs
   in zip xs <$> takeUnique n uis
