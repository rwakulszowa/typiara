{-# LANGUAGE ScopedTypeVariables #-}

module Typiara.Dag where

import Prelude hiding (null)

import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Tree (Tree(..))

import Data.Maybe (fromJust)
import Data.Traversable (mapAccumL)

import qualified Typiara.UniqueItemSource as UniqueItemSource

import Typiara.LeftOrRight (LeftOrRight)
import Typiara.Path (Path(..))
import Typiara.UniqueItemSource (UniqueItemSource)
import Typiara.Utils
  ( denumerateSequence
  , fromRight
  , insertIfNew
  , mapFromListWithKeyM
  , mapLeft
  , maybeError
  , tenumerate
  )

-- A subset of `Edge` that identifies its position in src.
-- Edges that differ only by `index` are considered different.
-- TODO: there must be a better name for this
-- TODO: store (Map srcId [dstId]) instead; encode indices in the list.
data EdgeSrc id =
  EdgeSrc
    { srcId :: id
    , index :: Int
    }
  deriving (Eq, Show, Ord)

type EdgeDst id = id

-- A Directed Acyclic Graph with indexed edges.
-- Indexing edges makes it possible to represent an exact tree shape, i.e. not only parent <-> child
-- relations, but also where the connection is defined (e.g. `b` is `a`'s first child, `c` is `a`'s second child).
--
-- TODO: smart constructor, reject cycles.
data Dag id =
  Dag
    { rootId :: id
    , edges :: Map (EdgeSrc id) (EdgeDst id)
    }
  deriving (Eq, Show)

empty :: (Ord id) => id -> Dag id
empty id = Dag id mempty

null :: Dag id -> Bool
null (Dag root edges) = Map.null edges

allIds :: (Ord id) => Dag id -> Set id
allIds (Dag rootId edges) =
  let idsFromEdges =
        concat
          [[srcId, dstId] | (EdgeSrc srcId index, dstId) <- Map.assocs edges]
   in Set.fromList (rootId : idsFromEdges)

hasId :: (Ord id) => Dag id -> id -> Bool
hasId dag id = id `Set.member` allIds dag

detectCycles :: (Ord id) => Dag id -> Maybe [id]
detectCycles dag@(Dag rootId edges) =
  case go [] rootId dag of
    Left cycle -> Just cycle
    Right () -> Nothing
  where
    go pathSoFar nodeId dag
          -- Cycles are returned as `Left`s to make short-circuiting easier.
          -- The top level function is responsible for inverting the functor, turning `Left` into `Just`.
      | nodeId `elem` pathSoFar = Left (nodeId : pathSoFar)
      | otherwise =
        () <$
        sequence
          ([ go (nodeId : pathSoFar) child dag
           | child <- fromJust $ children nodeId dag
           ])

-- Get ids of nodes reachable from `node`.
-- `Nothing` if lookup fails. Only possible if dag is invalid.
-- TODO: simplify after `Dag` internal types refactoring.
children :: (Ord id) => id -> Dag id -> Maybe [id]
children node dag =
  either (const Nothing) Just $
  denumerateSequence
    [ (index src, dst)
    | (src, dst) <- Map.assocs . edges $ dag
    , srcId src == node
    ]

-- Build a `Dag` from a tree of ids.
-- Nodes with the same `id` are considered linked and end up forming a single DAG node.
-- Rejects cycles and inconsistent trees (e.g. one `id` mapping to differing nodes).
newtype FromTreeErr a =
  Inconsistency a
  deriving (Eq, Show)

type Src id = id

type Dst id = id

fromTree :: (Ord id) => Tree id -> Either (FromTreeErr id) (Dag id)
fromTree tree
      -- Validation is done *before* unwrapping (src, [dsts]) to detect nodes of varying size.
 = do
  edgesPerNode :: Map (Src a) [Dst a] <-
    mapFromListWithKeyM rejectInconsistencies $ digTreeEdges tree
  individualEdges :: Map (EdgeSrc a) (EdgeDst a) <-
    mapFromListWithKeyM rejectEdgeInconsistencies $
    concatMap srcWithDstsToEdges $ Map.assocs edgesPerNode
  return $ Dag (Tree.rootLabel tree) individualEdges
  where
    digTreeEdges :: Tree a -> [(Src a, [Dst a])]
    digTreeEdges (Node value children) =
      (value, map Tree.rootLabel children) : concatMap digTreeEdges children
    rejectEdgeInconsistencies (EdgeSrc srcId _) = rejectInconsistencies srcId
    rejectInconsistencies key xs ys =
      if xs == ys
        then Right xs
        else Left $ Inconsistency key
    srcWithDstsToEdges (src, dsts) =
      [(EdgeSrc src i, dst) | (i, dst) <- tenumerate dsts]

data IntoTreeErr
  = IntoTreeErr
  | MissingEdge Path
  deriving (Eq, Show)

intoTree :: (Ord id) => Dag id -> Either IntoTreeErr (Tree id)
intoTree (Dag rootId edges) =
  intoTree' [] rootId (edgeMapToSrcChildrenMap edges)
  where
    edgeMapToSrcChildrenMap ::
         (Ord id) => Map (EdgeSrc id) (EdgeDst id) -> Map id [(Int, id)]
    edgeMapToSrcChildrenMap edges =
      Map.fromListWith
        mappend
        [ (srcId, [(index, dstId)])
        | (EdgeSrc srcId index, dstId) <- Map.assocs edges
        ]
    intoTree' path currentId edges = do
      childIds <-
        mapLeft (\i -> MissingEdge (path ++ [i])) $
        denumerateSequence $ Map.findWithDefault [] currentId edges
      childTrees <-
        sequence
          [ intoTree' (path ++ [i]) childId edges
          | (i, childId) <- zip [0 ..] childIds
          ]
      return $ Node currentId childTrees

at :: (Ord id) => Dag id -> Path -> Maybe id
at (Dag rootId edges) path = at' path rootId edges
  where
    at' [] currentId _ = Just currentId
    at' (p:ps) currentId edges = do
      dstId <- EdgeSrc currentId p `Map.lookup` edges
      at' ps dstId edges

-- The function must not cause conflicts.
-- TODO: pass an infinite sequence with duplicate checking instead?
mapIds :: (Ord id, Ord id') => (id -> id') -> Dag id -> Dag id'
mapIds f d =
  let (Right ret) = mapIdsWithConflictCheck f d
   in ret

-- Map all ids according to `fun`.
-- Returns `Left` upon detecting a conflict.
mapIdsWithConflictCheck ::
     (Ord id, Ord id') => (id -> id') -> Dag id -> Either id' (Dag id')
mapIdsWithConflictCheck fun (Dag rootId edges) = do
  edges <-
    sequence $
    Map.mapKeysWith detectConflicts (mapSrc fun) $ fmap (Right . fun) edges
  return $ Dag (fun rootId) edges
  where
    detectConflicts (Right x) (Right y) = Left x
    mapSrc fun (EdgeSrc id index) = EdgeSrc (fun id) index

data MergeErr id
  = PathNotFound
  | IdSourceErr (UniqueItemSource.NextErr id)
  | Cycle -- TODO: include the path, mapped to external arg ids
  deriving (Eq, Show)

if' :: (a -> Bool) -> (a -> a) -> (a -> a)
if' pred fun x =
  if pred x
    then fun x
    else x

-- Merge two `Dag`s together, shifting `right` nodes by `offset`.
merge ::
     (Ord id, Show id)
  => UniqueItemSource id
  -> Dag id
  -> Path
  -> Dag id
  -> Either (MergeErr id) (Dag id, Map (LeftOrRight id) id)
merge idSource left offset right = do
  (merged, diff) <- merge' left offset right
  let ids = allIds merged
  freshIds <- buildFreshIds (Set.size ids) idSource
  let idsMapping = Map.fromList $ zip (Set.elems ids) freshIds
  let idsMappingFun k = idsMapping Map.! k
  let combinedDiff =
        Map.unionWithKey rejectConflicts idsMapping (idsMappingFun <$> diff)
  return (mapIds idsMappingFun merged, combinedDiff)
    -- Reject if one key maps to different values.
  where
    rejectConflicts key v0 v1 =
      if v0 == v1
        then v0
        else error $ show ("DAG.combinedDiff conflict", key, v0, v1)
    buildFreshIds count source =
      mapLeft IdSourceErr $ UniqueItemSource.takeUnique count source

-- In some circumstances, the diff may contain a mapping of a value to itself.
-- There should be no assumptions regarding `merged` and `diff` being disjoint.
merge' ::
     (Ord id, Show id)
  => Dag id
  -> Path
  -> Dag id
  -> Either (MergeErr id) ( Dag (LeftOrRight id)
                          , Map (LeftOrRight id) (LeftOrRight id))
merge' left offset right =
  rejectCycles =<< do
    mappedLeft@(Dag leftRoot leftEdges) <- pure $ mapIds Left left
    mappedRight@(Dag rightRoot rightEdges) <- pure $ mapIds Right right
    leftNodeAtOffset <- maybeError PathNotFound $ mappedLeft `at` offset
    if null mappedRight
      then return (mappedLeft, Map.singleton rightRoot leftNodeAtOffset)
    -- Create an overlap by replacing the right root with an injected id in all edges.
    -- The overlap is then propagated across the whole graph.
      else let (resolved, diffMap) =
                 let (initialDiff, withInjectedOverlap) =
                       injectInitialOverlap leftNodeAtOffset mappedRight
                  in resolveOverlappingEdges initialDiff $
                     buildOverlapping [leftEdges, edges withInjectedOverlap]
            in return (rebuildDag resolved leftRoot, diffMap)
  where
    injectInitialOverlap injectedId dag =
      let droppedId = rootId dag
       in ( (droppedId, injectedId)
          , mapIds (if' (== droppedId) (const injectedId)) dag)
    -- Validate that the root is still there.
    -- TODO: more fancy checks?
    rebuildDag resolvedEdges cachedRoot =
      case EdgeSrc cachedRoot 0 `Map.lookup` resolvedEdges of
        Just _ -> Dag cachedRoot resolvedEdges
        Nothing -> error "Root is missing"
    rejectCycles (dag, diff) =
      case detectCycles dag of
        (Just _) -> Left Cycle
        Nothing -> Right (dag, diff)

-- Remove duplicate edges by merging duplicate `dst` nodes.
--
-- Edges are considered duplicate if > 1 edge originates from the same `EdgeSrc`.
-- In all such cases, `EdgeDst` nodes are considered linked and replaced with a single
-- value.
-- Map of updates performed during the operation.
-- Returned to the caller to let it further process changes.
type DiffMap id = Map (EdgeDst id) (EdgeDst id)

-- Individual update events. The same item may appear as a src in one element and as a dst in another.
-- The list has to be "flattened" before being converted to a `DiffMap`.
-- The most recent update is expected to be the first element.
type Updates a = [(a, a)]

data DiffMapFromUpdatesErr id
  = DuplicateUpdate (EdgeDst id)
  | OutOfOrderUpdate (EdgeDst id)
  deriving (Eq, Show)

-- TODO: move DiffMap to a module, implement a generic set of functions
-- to operate on applicatives with a diff
-- Process a sequence of ordered updates, building a DiffMap.
-- Processes items from left to right.
diffMapFromUpdates ::
     (Ord id)
  => Updates (EdgeDst id)
  -> Either (DiffMapFromUpdatesErr id) (DiffMap id)
diffMapFromUpdates = Monad.foldM processOne Map.empty
  where
    processOne diffMap (src, dst)
         -- Cannot map the same value twice.
         -- Identity mappings are assumed to have already been removed.
      | src `Map.member` diffMap = Left $ DuplicateUpdate src
         -- sources are expected to be processed before any item references them as a dst.
      | src `elem` Map.elems diffMap = Left $ OutOfOrderUpdate src
      | otherwise =
        Right $ insertIfNew' src (Map.findWithDefault dst dst diffMap) diffMap
    insertIfNew' k v m = fromJust $ insertIfNew k v m

-- Temporary type that defines a set potentially overlapping edges.
-- When all values are 1-element long lists, the type is considered non-overlapping
-- and can be turned into a `Dag`.
type OverlappingEdges id = Map (EdgeSrc id) (NonEmpty (EdgeDst id))

buildOverlapping ::
     (Ord id) => [Map (EdgeSrc id) (EdgeDst id)] -> OverlappingEdges id
buildOverlapping = Map.unionsWith (<>) . fmap (fmap (:| []))

-- Resolved type.
type NonOverlappingEdges id = Map (EdgeSrc id) (EdgeDst id)

intoNonOverlapping :: OverlappingEdges id -> Maybe (NonOverlappingEdges id)
intoNonOverlapping = traverse singletonOrNothing
  where
    singletonOrNothing (x :| []) = Just x
    singletonOrNothing _ = Nothing

-- If no duplicates are found, returns `Nothing` and the argument, unmodified.
-- Otherwise, returns the pair of duplicates and removes one of them from the map.
popOne ::
     OverlappingEdges id
  -> (Maybe (EdgeDst id, EdgeDst id), OverlappingEdges id)
popOne = mapAccumL popFirstOverlapping Nothing
  where
    popFirstOverlapping ::
         Maybe (a, a) -> NonEmpty a -> (Maybe (a, a), NonEmpty a)
              -- Pop only if:
              --   - nothing has been popped yet
              --   - the item has at least 2 elements
    popFirstOverlapping Nothing (x :| (y:ys)) = (Just (x, y), y :| ys)
    popFirstOverlapping state elem = (state, elem)

-- Replace a single id with a new value, both in keys and values.
-- May merge multiple keys together.
--
-- Returns the merged map and replacement pair: `(oldId, newId)`.
replaceId ::
     (Eq id, Ord id)
  => id
  -> id
  -> OverlappingEdges id
  -> (OverlappingEdges id, (id, id))
replaceId oldId newId overlappingEdges =
  let updatedContainer =
        Map.mapKeysWith (<>) (updateKey oldId newId) $
        updateValues oldId newId <$> overlappingEdges
   in (updatedContainer, (oldId, newId))
  where
    updateValues oldId newId = fmap $ if' (== oldId) (const newId) -- TODO: nub / just use a Set
    updateKey oldId newId (EdgeSrc keyId index) =
      if keyId == oldId
        then EdgeSrc newId index
        else EdgeSrc keyId index

resolveOverlappingEdges ::
     (Ord id, Show id)
  => (id, id)
  -> OverlappingEdges id
  -> (NonOverlappingEdges id, DiffMap id)
resolveOverlappingEdges (initialReplacedNode, initialInjectedNode) overlappingEdges =
  fromJust $ do
    (resolvedEdges, updates) <-
      pure $
      resolve' overlappingEdges [(initialReplacedNode, initialInjectedNode)]
    nonOverlappingEdges <- intoNonOverlapping resolvedEdges
    let diffMap = fromRight . diffMapFromUpdates $ updates
    return (nonOverlappingEdges, diffMap)
  where
    resolve' ::
         (Ord id, Show id)
      => OverlappingEdges id
      -> Updates (EdgeDst id)
      -> (OverlappingEdges id, Updates (EdgeDst id))
    resolve' overlappingEdges updates =
      case popOne overlappingEdges of
        (Just (x, y), remainingEdges)
          -- Ignore identity mappings. They have no effect anyway.
          | x == y -> resolve' remainingEdges updates
          | otherwise ->
            let (updatedEdges, (erasedId, erasedIdsReplacement)) =
                  replaceId x y remainingEdges
             in resolve'
                  updatedEdges
                  ((erasedId, erasedIdsReplacement) : updates)
        (Nothing, overlappingEdgesWithNoDuplicates) ->
          (overlappingEdgesWithNoDuplicates, updates)
