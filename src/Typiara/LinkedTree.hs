{-# LANGUAGE ScopedTypeVariables #-}

module Typiara.LinkedTree
  ( LinkedTree
  , linkedTree
  , shape
  , values
  , ro
  , singleton
  , triple
  , linkedTriple
  , intoTree
  , fromTree
  , expand
  , FromTreeErr(..)
  , MergeErr(..)
  , merge
  , shift
  , draw
  , scan
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Tree (Tree(..))

import Control.Monad.Zip (munzip, mzip)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Traversable (mapAccumL)

import qualified Typiara.Dag as Dag
import qualified Typiara.Link as Link
import qualified Typiara.UniqueItemSource as UniqueItemSource

import Typiara.LeftOrRight (LeftOrRight)
import Typiara.Link (Link(..))
import Typiara.Path (Path(..))
import Typiara.Utils
  ( allStrings
  , atPath
  , fromRight
  , insertIfNew
  , invertMap
  , mapKeysRejectConflicts
  , mapLeft
  , mapSnd
  , scanTree
  , sequenceFst
  , sequenceSnd
  )

data LinkedTree a =
  LinkedTree
    { shape :: Tree Link
    , values :: Map Link a
    }
  deriving (Show)

-- Read-only variant of the above.
-- Can be, to some extent, pattern matched on.
ro (LinkedTree shape values) = (shape, values)

instance Eq a => Eq (LinkedTree a) where
  (==) l r =
    let l' = refreshLinks l
        r' = refreshLinks r
     in ro l' == ro r'

instance Functor LinkedTree where
  fmap f (LinkedTree shape values) = LinkedTree shape (fmap f values)

instance Foldable LinkedTree where
  foldMap f linkedTree = foldMap f (values linkedTree)

instance Traversable LinkedTree where
  traverse f (LinkedTree shape values) = LinkedTree shape <$> traverse f values

null :: LinkedTree a
null = LinkedTree (Node (Link "null") []) Map.empty

singleton :: a -> LinkedTree a
singleton a = LinkedTree (Node root []) (Map.singleton root a)
  where
    root = Link "root"

-- Build a linked tree of 3 nodes:
--   - root
--     - left
--     - right
-- Link ids are automatically generated.
triple :: a -> a -> a -> LinkedTree a
triple root left right =
  let shape = Node rootLink [Node leftLink [], Node rightLink []]
      values =
        Map.fromList [(rootLink, root), (leftLink, left), (rightLink, right)]
   in fromRight $ linkedTree shape values
  where
    leftLink = Link "left"
    rightLink = Link "right"
    rootLink = Link "root"

-- Same as `triple`, except `left` and `right` are linked.
linkedTriple :: a -> a -> LinkedTree a
linkedTriple root leaf =
  let shape = Node rootLink [Node leafLink [], Node leafLink []]
      values = Map.fromList [(rootLink, root), (leafLink, leaf)]
   in fromRight $ linkedTree shape values
  where
    leafLink = Link "leaf"
    rootLink = Link "root"

data LinkedTreeConstructorError =
  LinksOutOfSync
    { shapeLinks :: Set Link
    , valuesLinks :: Set Link
    }
  deriving (Eq, Show)

linkedTree ::
     Tree Link -> Map Link a -> Either LinkedTreeConstructorError (LinkedTree a)
linkedTree shape values =
  let shapeLinks = Set.fromList $ Tree.flatten shape
      valuesLinks = Set.fromList $ Map.keys values
   in if shapeLinks == valuesLinks
        then Right $ LinkedTree shape values
        else Left $ LinksOutOfSync shapeLinks valuesLinks

intoTree :: LinkedTree a -> Tree (Link, a)
intoTree (LinkedTree shape values) =
  let fun link = sequenceSnd (link, Map.lookup link values)
   in fromJust $ traverse fun shape

expand :: LinkedTree a -> Tree a
expand = fmap snd . intoTree

draw :: (Show a) => LinkedTree a -> String
draw = Tree.drawTree . fmap show . intoTree

data FromTreeErr a =
  ConflictingValues Link (a, a)
  deriving (Eq, Show)

fromTree :: (Eq a) => Tree (Link, a) -> Either (FromTreeErr a) (LinkedTree a)
fromTree tree = do
  (linkValueMap, shape) <-
    sequenceFst $ mapAccumL extractAndStoreValue (Right Map.empty) tree
  return $ LinkedTree shape linkValueMap
  where
    extractAndStoreValue ::
         (Eq a)
      => Either (FromTreeErr a) (Map Link a)
      -> (Link, a)
      -> (Either (FromTreeErr a) (Map Link a), Link)
    extractAndStoreValue (Left err) (link, _) = (Left err, link)
    extractAndStoreValue (Right acc) (link, value) =
      case Map.lookup link acc of
        (Just storedValue) ->
          if storedValue == value
            then (Right acc, link)
            else (Left (ConflictingValues link (storedValue, value)), link)
        Nothing ->
          case insertIfNew link value acc of
            (Just inserted) -> (Right inserted, link)
            Nothing -> error "Insertion failed"

shift :: Path -> LinkedTree a -> Maybe (LinkedTree a)
shift offset (LinkedTree shape values) = do
  shiftedShape <- atPath offset shape
  return $
    LinkedTree shiftedShape (values `Map.restrictKeys` linkSet shiftedShape)
  where
    linkSet = Set.fromList . Tree.flatten

data MergeErr a
  = LookupError (LeftOrRight Link)
  | DagMergeErr (Dag.MergeErr Link)
  | DagIntoTreeErr Dag.IntoTreeErr
  | DagFromTreeErr (Dag.FromTreeErr Link)
  deriving (Eq, Show)

-- Merge two trees into one.
-- `guest`'s root is merged with `host's node at `mergeLocation`.
merge ::
     (Show a)
  => LinkedTree a
  -> Path
  -> LinkedTree a
  -> Either (MergeErr a) (LinkedTree (NonEmpty a))
merge (LinkedTree hostShape hostValues) mergeLocation (LinkedTree guestShape guestValues) = do
  hostDag <- mapLeft DagFromTreeErr $ Dag.fromTree hostShape
  guestDag <- mapLeft DagFromTreeErr $ Dag.fromTree guestShape
  ((mergedShape, linkMapping) :: (Tree Link, Map (LeftOrRight Link) Link)) <-
    mergeDagAndUpdateIds hostDag mergeLocation guestDag
  (combinedValues :: Map Link (NonEmpty a)) <-
    mapLeft LookupError $
    traverse sequence $
    fmap (leftOrRightLookup hostValues guestValues) <$> invertMap linkMapping
  return $ LinkedTree mergedShape combinedValues
  where
    leftOrRightLookup ::
         (Ord k)
      => Map k v
      -> Map k v
      -> LeftOrRight k
      -> Either (LeftOrRight k) v
    leftOrRightLookup leftMap rightMap lrKey =
      maybe (Left lrKey) Right $
      case lrKey of
        Right key -> rightMap Map.!? key
        Left key -> leftMap Map.!? key
    mergeDagAndUpdateIds host location guest = do
      (shape, diff) <-
        mapLeft DagMergeErr $
        Dag.merge Link.uniqueLinkSource host location guest
      updatedShape <- mapLeft DagIntoTreeErr (Dag.intoTree shape)
      return (updatedShape, diff)

scan :: (Eq a, Eq b, Show b) => (Tree a -> b) -> LinkedTree a -> LinkedTree b
scan f =
  fromRight . fromTree . uncurry mzip . mapSnd (scanTree f) . munzip . intoTree

-- Generate new set of links.
-- Useful when exposing data to the external world, e.g. when comparing two different instances.
-- Under no circumstances should crash. All unsafe operations depend on assumptions guaranteed by the smart constructor.
-- TODO: consider refreshing before returning any internal data.
refreshLinks :: LinkedTree a -> LinkedTree a
refreshLinks (LinkedTree shape values) =
  let allLinksInPreOrder = List.nub . Tree.flatten $ shape
      oldToNewMap =
        Map.fromList . fromRight $
        allLinksInPreOrder `UniqueItemSource.zipUnique` Link.uniqueLinkSource
      refresh k = oldToNewMap Map.! k
   in fromRight $
      linkedTree
        (refresh <$> shape)
        (fromJust $ mapKeysRejectConflicts refresh values)
