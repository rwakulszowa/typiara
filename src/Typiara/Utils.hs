{-# LANGUAGE ScopedTypeVariables #-}

module Typiara.Utils where

import qualified Data.IntMap.Strict       as IM
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Tree                as Tree

import           Data.Bifunctor           (first, second)
import           Data.List.NonEmpty       (NonEmpty (..))
import           Data.Map                 (Map)
import           Data.Set                 (Set)
import           Data.Tree                (Tree (..))
import           Typiara.Data.LeftOrRight (LeftOrRight)

import           Control.Monad            (foldM)
import           Control.Monad.Zip        (munzip, mzip)
import           Data.Functor             (($>))
import           Data.List                (find, sortOn)
import           Data.Maybe               (fromJust, maybe)
import           Data.Semigroup           ((<>))
import           Data.Traversable         (mapAccumL)

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

allStrings = [c : s | s <- "" : allStrings, c <- ['a' .. 'z']]

fromRight (Right x)  = x
fromRight (Left err) = error $ show err

fromJustOrError :: String -> Maybe a -> a
fromJustOrError e = fromRight . maybeError e

maybeError :: err -> Maybe a -> Either err a
maybeError err = maybe (Left err) Right

invertMaybe :: Maybe () -> Maybe ()
invertMaybe Nothing   = Just ()
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
    pair [] y     = ([], Nothing)
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
atPath [] tree                  = Just tree
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

mapFromListWithKeyM ::
     (Ord k, Monad m) => (k -> v -> v -> m v) -> [(k, v)] -> m (Map k v)
mapFromListWithKeyM f assocs =
  sequence $ Map.fromListWithKey (liftM' f) $ map (second pure) assocs
  where
    liftM' f k x y = do
      x <- x
      y <- y
      f k x y

mapKeysRejectConflicts ::
     (Ord k, Ord k') => (k -> k') -> Map k v -> Maybe (Map k' v)
mapKeysRejectConflicts f =
  sequence . Map.mapKeysWith rejectConflict f . fmap Just
  where
    rejectConflict _ _ = Nothing

-- | Map keys through `f`.
-- Conflicts are detected by checking sizes - any conflict will make the size decrease.
mapIKeysRejectConflicts :: (Int -> Int) -> IM.IntMap v -> Maybe (IM.IntMap v)
mapIKeysRejectConflicts f m =
  let m' = IM.mapKeys f m
   in if IM.size m' == IM.size m
        then Just m'
        else Nothing

pop :: (Ord k) => Map k v -> k -> Maybe (v, Map k v)
pop m k =
  case m Map.!? k of
    Nothing  -> Nothing
    (Just v) -> Just (v, k `Map.delete` m)

popN m =
  foldM
    (\(m, acc) k -> do
       (v, m') <- m `pop` k
       return (m', acc ++ [v]))
    (m, mempty)

-- | Given a map from k to v and a set of expected ks, turn a map
-- into a function that never fails to find a value.
-- The returned function should only be called on values included in `ks`.
buildLookupF :: (Ord k) => Map k v -> Set k -> Either (NonEmpty k) (k -> v)
buildLookupF kv keys =
  let missingKeys =
        (NonEmpty.nonEmpty . Set.elems)
          (Set.fromList (Map.keys kv) `Set.difference` keys)
   in case missingKeys of
        Nothing   -> Right (kv Map.!)
        (Just mk) -> Left mk

-- | Replace old values with a fresh set.
-- New items are picked in topological order.
refresh :: (Traversable t, Ord a) => [b] -> t a -> (Map a b, t b)
refresh bs t =
  let ((diff, _), t') = mapAccumL getOrTake (Map.empty, bs) t
   in (diff, t')
  where
    getOrTake (seen, bs) a =
      case a `Map.lookup` seen of
        (Just b) -> ((seen, bs), b)
        Nothing ->
          let (b:bs') = bs
           in ((Map.insert a b seen, bs'), b)

irefresh :: (Traversable t) => t Int -> (IM.IntMap Int, t Int)
irefresh t =
  let ((diff, _), t') = mapAccumL getOrTake (mempty, [0 ..]) t
   in (diff, t')
  where
    getOrTake (seen, is) a =
      case a `IM.lookup` seen of
        (Just b) -> ((seen, is), b)
        Nothing ->
          let (b:is') = is
           in ((IM.insert a b seen, is'), b)

refreshVs zero = uncurry mzip . first (snd . refresh [zero ..]) . munzip
