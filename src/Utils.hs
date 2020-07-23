{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Set (Set)
import Data.Tree (Tree(..))

import Control.Monad (foldM)
import Control.Monad.Zip (mzip)
import Data.Function (on)
import Data.Functor (($>))
import Data.List ((\\), find, groupBy, partition, sortOn)
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Semigroup ((<>))
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)

import LeftOrRight
import OneOrTwo

maybeAt :: [a] -> Int -> Maybe a
maybeAt list index = snd <$> (find ((== index) . fst) . zip [0 ..] $ list)

-- Map each (x:xs) list to (x, xs) pairs, grouped by x.
-- Empty lists are filtered out.
hoistHeads :: (Ord k) => [([k], v)] -> [(k, [([k], v)])]
hoistHeads lists =
  Map.toAscList $
  foldr
    (\(x, xs) acc -> Map.insertWith (++) x [xs] acc)
    Map.empty
    [(x, (xs, value)) | (x:xs, value) <- lists]

enumeratingTree :: Tree [Int]
enumeratingTree =
  let f path = (path, [path ++ [i] | i <- [0 ..]])
   in Tree.unfoldTree f []

enumerateTree = mzip enumeratingTree

allStrings = [c : s | s <- "" : allStrings, c <- ['a' .. 'z']]

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

fromRight (Right x) = x
fromRight (Left err) = error $ show err

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst fun (a, b) = (fun a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd fun (a, b) = (a, fun b)

fromJustOrError :: String -> Maybe a -> a
fromJustOrError _ (Just x) = x
fromJustOrError err Nothing = error err

sequenceFst :: (Monad m) => (m a, b) -> m (a, b)
sequenceFst (a, b) = do
  a' <- a
  return (a', b)

sequenceSnd :: (Monad m) => (a, m b) -> m (a, b)
sequenceSnd x = swap <$> (sequenceFst . swap $ x)

maybeError :: err -> Maybe a -> Either err a
maybeError err = maybe (Left err) Right

invertMaybe :: Maybe () -> Maybe ()
invertMaybe Nothing = Just ()
invertMaybe (Just ()) = Nothing

-- Updates value associated with `key`.
-- If no element is found, returns `Nothing`.
lookupAndUpdate :: (Ord k) => k -> (v -> v) -> Map k v -> Maybe (Map k v)
lookupAndUpdate k f map = do
  v <- Map.lookup k map
  return $ Map.adjust f k map

-- Inserts a new item into the map only if no item is stored under `k`.
-- If `k` already exists, returns `Nothing`.
insertIfNew :: (Ord k) => k -> v -> Map k v -> Maybe (Map k v)
insertIfNew k v map = do
  invertMaybe $ () <$ Map.lookup k map
  return $ Map.insert k v map

-- Traversable zip
-- https://stackoverflow.com/a/41523456
tzipWith :: Traversable t => (a -> b -> c) -> [a] -> t b -> Maybe (t c)
tzipWith f xs = sequenceA . snd . mapAccumL pair xs
  where
    pair [] y = ([], Nothing)
    pair (x:xs) y = (xs, Just (f x y))

tzip :: Traversable t => [a] -> t b -> Maybe (t (a, b))
tzip = tzipWith (,)

tenumerate :: Traversable t => t b -> t (Int, b)
tenumerate = fromJust . tzip [0 ..]

denumerate :: Traversable t => t (Int, b) -> t b
denumerate = fmap snd

-- Turn an (index, value) sequence into a list sorted by `index`.
-- `Left` if any index is missing or overlapping.
denumerateSequence :: [(Int, a)] -> Either Int [a]
denumerateSequence xs = reverse . snd <$> foldM takeOne (0, []) (sortOn fst xs)
  where
    takeOne :: (Int, [a]) -> (Int, a) -> Either Int (Int, [a])
    takeOne (expectedIndex, acc) (currentIndex, value)
      | expectedIndex == currentIndex = Right (expectedIndex + 1, value : acc)
      | otherwise = Left expectedIndex

-- Tree traversal.
atPath :: [Int] -> Tree a -> Maybe (Tree a)
atPath [] tree = Just tree
atPath (p:ps) (Node _ children) = maybeAt children p >>= atPath ps

-- Zip the common part, return the remainder.
-- The remainder is `Left` if it comes from the first argument.
losslessZip :: [a] -> [a] -> ([(a, a)], LeftOrRight [a])
losslessZip x y =
  let zippedPart = zip x y
      remainder =
        if length x > length y
          then Left $ drop (length y) x
          else Right $ drop (length x) y
   in (zippedPart, remainder)

invertMap :: (Ord k, Ord v) => Map k v -> Map v (NonEmpty k)
invertMap =
  Map.fromListWith (<>) . fmap (swap . (\(k, v) -> (k :| [], v))) . Map.assocs

mapFromListWithKeyM ::
     (Ord k, Monad m) => (k -> v -> v -> m v) -> [(k, v)] -> m (Map k v)
mapFromListWithKeyM f assocs =
  sequence $ Map.fromListWithKey (liftM' f) $ map (mapSnd pure) assocs
  where
    liftM' f k x y = do
      x <- x
      y <- y
      f k x y

scanTree :: (Tree a -> b) -> Tree a -> Tree b
scanTree f node@(Node _ children) = Node (f node) (scanTree f <$> children)
