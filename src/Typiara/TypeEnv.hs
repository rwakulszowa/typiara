{-# LANGUAGE DeriveTraversable, DeriveDataTypeable #-}

module Typiara.TypeEnv where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Zip (munzip, mzip)
import Data.Bifunctor (bimap, first, second)
import Data.Data (Data, Typeable)
import Data.Foldable (foldlM, foldrM, toList)
import Data.Function (on)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Traversable (mapAccumL)
import qualified Data.Tree as Tree
import Data.Tuple (swap)
import Text.Read (readMaybe)

import Typiara.Data.Tagged (Tagged(..))
import Typiara.FT (FT(..))
import Typiara.Fix
import Typiara.Typ (Typ(..), UnifyError(..), UnifyResult(..))
import qualified Typiara.Utils as Utils

-- | Type wrapper for identifiers with one special value.
data RootOrNotRoot a
  = Root
  | NotRoot a
  deriving (Eq, Show, Ord, Functor, Traversable, Foldable, Data, Typeable)

instance (Enum a) => Enum (RootOrNotRoot a) where
  toEnum 0 = Root
  toEnum i = NotRoot (toEnum (i - 1))
  fromEnum Root = 0
  fromEnum (NotRoot a) = fromEnum a + 1

-- | Generic storage for type variables.
-- Most operations are defined on `TypeEnv` directly, which is a wrapper of 
-- this type. In some circumstances, `TypeEnv` invariants may be violated -
-- those scenarios should operate on `TypeVarMap`.
-- After fixing violations, `TypeEnv` should be reintroduced.
--
-- TLDR: Raw type variable storage. If doing unsafe stuff, fetch raw data,
-- do stuff and wrap it with `TypeEnv` when done.
type TypeVarMap t v = Map.Map (RootOrNotRoot v) (FT t v)

data Indexed i t a =
  Indexed i (t a)
  deriving (Functor)

-- | Rebuild a tree from the map.
-- NOTE: not really used anywhere (yet?).
--
-- This fuction builds a directed graph from the map representation.
-- Cycle detection can be done by attempting to build a tree from the graph,
-- tracking paths leading to each node. Upon detecting a cycle, the function
-- should return early.
-- The attempt has been abandoned because a simpler solution exists, but the
-- code looks pretty so it's still here.
recompose ::
     (Ord v, Functor t)
  => TypeVarMap t v
  -> Fix (Indexed (RootOrNotRoot v) (FT t))
recompose tv = Fix (Indexed Root (go Root))
  where
    go k =
      let node = tv Map.! k
       in (\k ->
             let k' = NotRoot k
              in Fix . Indexed k' . go $ k') <$>
          node

-- | Traverse the structure from the root, aggregating path seen so far.
-- Bail upon finding the same node id twice in one path.
-- TODO: rewrite / provide an alternative in terms of `recompose`
findCycles ::
     (Foldable t, Ord v, Eq v) => TypeVarMap t v -> Maybe [RootOrNotRoot v]
findCycles tv =
  case go [] Root of
    Left cycle -> Just cycle
    Right () -> Nothing
  where
    get' = (tv Map.!)
    go path v
      | v `elem` path = Left (v : path)
      | otherwise =
        () <$ sequence ([go (v : path) (NotRoot vc) | vc <- toList (get' v)])

-- | Map `a`s to `b`s.
-- `b` *must* generate unique values on each `succ` call.
refresh ::
     (Functor t, Foldable t, Ord a, Ord b, Enum b)
  => TypeVarMap t a
  -> (Map.Map a b, TypeVarMap t b)
refresh t =
  let mapping = Map.fromList $ Set.elems (allVars t) `zip` [(toEnum 0) ..]
   in (mapping, fmapTVs (mapping Map.!) t)

-- | Dig all stored type variable names.
allVars :: (Ord a, Foldable t) => TypeVarMap t a -> Set.Set a
allVars tvs =
  (Set.fromList . concatMap toList . Map.keys) tvs `mappend`
  (Set.fromList . concatMap toList . Map.elems) tvs

-- | Note on pseudo instances.
-- The type doesn't satisfy `fmap` because it uses `a` twice in its signature.
-- There's also an additional requirement on fmap's `f`, namely, that it cannot
-- cause any conflicts.
-- As a result, the type is not a proper `Functor`.
--
-- | `fmap` substitute.
-- `f` *must not* cause conflicts.
fmapTVs ::
     (Functor t, Ord a, Ord b) => (a -> b) -> TypeVarMap t a -> TypeVarMap t b
fmapTVs f tvs = mapValue <$> mapKeysRejectConflicts mapKey tvs
  where
    mapKey = fmap f
    mapValue = fmap f
    mapKeysRejectConflicts f = Maybe.fromJust . Utils.mapKeysRejectConflicts f

-- | All data required to fully define a type.
-- Note, that type variable names are local. When merging multiple instances,
-- make sure to generate new names.
--
-- Both root and non-root items are stored in the same container.
-- In most use cases, the ability to point to the root item is not required.
-- `get` provides a friendly interface for those scenarios.
--
-- `Num` == `[(Root, T Num)]`
-- `Bool -> Bool` == `[(Root, F a a), (a, T Bool)]`
newtype TypeEnv t v =
  TypeEnv
    { unTypeEnv :: TypeVarMap t v
    }
  deriving (Show, Ord)

-- | Build an instance from shape and constraints.
-- Rejects out of sync inputs (i.e. such that the shape doesn't match constraint keys).
--
-- `Num -> Num` == `(f [a, a], [(f, F), (a, Num)])`
fromTree ::
     (Ord v, Tagged t v, Enum v)
  => Tree.Tree (RootOrNotRoot v)
  -> Map.Map (RootOrNotRoot v) String
  -> Either (FromTreeError v) (TypeEnv t v)
fromTree shape constraints = do
  ((), s) <- runWriterT (go shape)
  return (TypeEnv (Map.fromList s))
  where
    get v = Utils.maybeError (VarNotFound v) (constraints Map.!? v)
    untag :: (Tagged t a) => String -> [a] -> Either (FromTreeError a) (t a)
    untag t vs = Utils.maybeError (UntagError t vs) (fromTag t vs)
    rejectRoot Root = Left BadShape
    rejectRoot (NotRoot a) = Right a
    -- | Iterate the tree, reading items in the process and storing processed items in a list.
    -- The list will be later compressed into a map.
    go (Tree.Node v vs) = do
      c <- lift (get v)
      vs' <- lift (traverse rejectRoot . fmap Tree.rootLabel $ vs)
      t <- lift (untag c vs')
      tell [(v, t)]
      mapM_ go vs

data FromTreeError a
  = VarNotFound (RootOrNotRoot a)
  | UntagError String [a]
  | BadShape
  -- ^ Thrown if `Root` appears in a non-root position.
  deriving (Eq, Show)

-- | A more convenient variant of `fromTree`, that doesn't require wrapping
-- ids with `RootOrNotRoot`.
fromEnumTree ::
     (Ord a, Enum a, Ord v, Enum v, Tagged t v)
  => Tree.Tree a
  -> Map.Map a String
  -> Either (FromEnumTreeError a v) (TypeEnv t v)
fromEnumTree shape constraints = do
  let (diff, shape') = Utils.refresh [Root ..] shape
  constraints' <- updateKeys diff constraints
  first FromTreeErr (fromTree shape' constraints')
  where
    updateKeys diff m =
      pure (Map.assocs m) >>= traverse f >>= Utils.mapFromListWithKeyM reject
      where
        f (k, v) =
          case diff Map.!? k of
            (Just k') -> Right (k', v)
            Nothing -> Left (ShapeConstraintsOutOfSync k)
        reject k _ _ = Left (KeyOverlap k)

data FromEnumTreeError a v
  = KeyOverlap (RootOrNotRoot v)
  -- ^ Happens only on bad implementations of `Enum`.
  | ShapeConstraintsOutOfSync a
  | FromTreeErr (FromTreeError v)
  deriving (Eq, Show)

-- | Reverse of `fromEnumTree`.
decompose ::
     (Ord a, Enum a, Ord v, Enum v, Tagged t v, Foldable t)
  => TypeEnv t v
  -> (Tree.Tree a, Map.Map a String)
decompose = stripTags . refresh' . shape
  where
    refresh' = refreshVs (toEnum 0)
    stripTags = swap . mapAccumL f Map.empty
      where
        f s (v, tag) =
          let s' =
                case s Map.!? v of
                  Nothing -> Map.insert v tag s
                  (Just tag')
                    | tag' == tag -> s
                  (Just tag') -> error "Tags out of sync"
           in (s', v)

refreshVs zero = uncurry mzip . first (snd . Utils.refresh [zero ..]) . munzip

-- | Rebuild a tree.
-- Conversion is lossless - the tree contains information about shape, links
-- and constraints.
shape ::
     (Ord v, Foldable t, Tagged t v)
  => TypeEnv t v
  -> Tree.Tree (RootOrNotRoot v, String)
shape te = Tree.unfoldTree f Root
  where
    f v =
      let n = Maybe.fromJust (getR te v)
       in ((v, tag n), NotRoot <$> toList n)

instance (Ord v, Foldable t, Tagged t v) => Eq (TypeEnv t v) where
  (==) = (==) `on` refreshVs 0 . shape

data UnifyEnvError t v
  = KeyNotFound (RootOrNotRoot v)
  | UnifyError (UnifyError t v)
  deriving (Eq, Show)

-- | Single element instance.
singleton :: FT t v -> TypeEnv t v
singleton t = TypeEnv (Map.singleton Root t)

-- | Get the root item.
-- Crashes on failure. A root item should always exist.
getRoot :: (Ord v) => TypeEnv t v -> FT t v
getRoot t = unTypeEnv t Map.! Root

-- | Get a non-root item.
get :: (Ord v) => TypeEnv t v -> v -> Maybe (FT t v)
get t k = unTypeEnv t Map.!? NotRoot k

getR t Root = Just (getRoot t)
getR t (NotRoot k) = get t k

-- | Replace type variable stored under `k` with `v`.
-- No recursive updates.
-- TODO: investigate how likely it is to leave the instance in and invalid
-- state after this call (orphans / cycles). Consider cleaning up after the
-- operation.
replace :: (Ord v) => RootOrNotRoot v -> FT t v -> TypeEnv t v -> TypeEnv t v
replace k v = TypeEnv . Map.alter f k . unTypeEnv
  where
    f Nothing = error "key not found"
    f (Just _) = Just v

-- | Merge two instances, injecting an id from one item into the other.
-- Left root is left intact.
--
-- This operation is performed when two separate types, represented as
-- `TypeEnv`s are merged, e.g. when we know from one source that a type is both
-- `a -> a -> a` and `Num -> b`.
unifyEnv ::
     (Typ t, Functor t, Foldable t, Ord v, Enum v)
  => RootOrNotRoot v
  -> TypeEnv t v
  -> TypeEnv t v
  -> Either (UnifyEnvError t v) (TypeEnv t v)
unifyEnv leftIdToMerge (TypeEnv a) (TypeEnv b) =
  let leftTvs = annotateMap Left (Left . NotRoot) a
      rightTvs = annotateMap Right (Right . NotRoot) b
      -- ^ Annotate all items with sides to avoid conflicts.
      -- Mapping is done on a Map, rather than on a `TypeEnv`, because the
      -- `Root`s should be annotated as well; Mapping `TypeEnv`s affects
      -- non-root items only.
      mergedTVs =
        let combined = (leftTvs `mappend` rightTvs)
            (leftRoot, nonRoots) =
              Maybe.fromJust (combined `Utils.pop` Left Root)
            -- ^ Pop the left root - it will become the new root.
            -- Right root becomes a regular item. At this point it is still
            -- annotated with `Right Root`, but that will disappear after
            -- refreshing ids.
         in (Map.mapKeys NotRoot nonRoots `mappend` Map.singleton Root leftRoot)
      (mapping, refreshed) = refresh mergedTVs
      -- `Left Root` was popped before refreshing, therefore it won't be found
      -- in `mapping`.
      maptv a =
        case a of
          (Left Root) -> Root
          x -> NotRoot (mapping Map.! x)
   in clean <$>
      unifyVars
        (TypeEnv refreshed)
        (maptv (Left leftIdToMerge), maptv (Right Root))
  where
    annotateMap fk fv m =
      Maybe.fromJust (fmap (fmap fv) <$> Utils.mapKeysRejectConflicts fk m)
    -- ^ Key mapping won't crash as long as the function is an annotation (adds
    -- new data without dropping any).

-- | Unify two variables in a given environment.
-- TODO: there should be an option to modify keys, not just values.
-- `F a b` may need to change to `F a a` in certain circumstances, namely,
-- when it gets merged with a linked function. Modifying keys is risky, as it
-- may create orphans. Change the mechanism to keep old values and create new
-- variables whenever necessary. Old values should point to the new ones, e.g.
-- `F a b + unify a b = F c c + Link a c + Link b c`
unifyVars ::
     (Ord v, Typ t)
  => TypeEnv t v
  -> (RootOrNotRoot v, RootOrNotRoot v)
  -> Either (UnifyEnvError t v) (TypeEnv t v)
unifyVars ti (x, y) = do
  tx <- ti `find` x
  ty <- ti `find` y
  unifyResult <- first UnifyError (tx `unify` ty)
  case unifyResult of
    (Unified t) -> Right (replace x t . replace y t $ ti)
    (TypeVarsToUnify ts) -> foldUnify ti (bimap NotRoot NotRoot <$> ts)
  where
    find t k = Utils.maybeError (KeyNotFound k) (t `getR` k)
    foldUnify ti pairs = foldlM unifyVars ti pairs

-- | `TypeEnv` representing a chain of functions.
buildFunEnv :: (Ord v, Enum v) => Int -> TypeEnv t v
buildFunEnv arity =
  let (t:ts) = generateFunctionTypes arity (toEnum 0)
   in TypeEnv (Map.fromList ((Root, snd t) : (first NotRoot <$> ts)))
  where
    generateFunctionTypes 0 v = [(v, Nil)]
    -- ^ The last return value is a `Nil`.
    generateFunctionTypes remaining v =
      let vArg = succ v
          vRet = succ vArg
          node = (v, F vArg vRet)
          arg = (vArg, Nil)
       in node : arg : generateFunctionTypes (remaining - 1) vRet

-- | Build a function `tA -> tB`, where the argument is stored under `a` and
-- the result under `b`.
funT (a, tA) (b, tB) =
  TypeEnv (Map.fromList [(Root, F a b), (NotRoot a, tA), (NotRoot b, tB)])

-- | Same as `funT`, except ids are generated automatically.
funT' tA tB =
  let a = toEnum 0
      b = succ a
   in funT (a, tA) (b, tB)

-- | Traverse the tree, returning n'th return node.
-- Will crash if `arity < n`.
nthFunNode :: (Ord v) => TypeEnv t v -> Int -> FT t v
nthFunNode t n = go (getRoot t) n
  where
    get' = Maybe.fromJust . get t
    go t 0 = t
    go (F _ ret) n = go (get' ret) (n - 1)

nthArgId t n =
  let (F arg _) = nthFunNode t n
   in arg

-- | Decompose the object into standalone arg and fun pieces.
popArg typeInfo =
  let (F argId retId) = nthFunNode typeInfo 0
      pick' = pick typeInfo . NotRoot
   in (pick' argId, pick' retId)

-- | Pick a minimal subset required to represent a type under `id`.
-- The `id` node becomes the new root.
-- When called on `Root`, the function will shake off any redundant type vars.
pick :: (Foldable t, Ord v) => TypeEnv t v -> RootOrNotRoot v -> TypeEnv t v
pick base id =
  let r = get' id
   in TypeEnv
        ((unTypeEnv base `Map.restrictKeys` Set.fromList (NotRoot <$> dig r)) `mappend`
         Map.singleton Root r)
  where
    dig t =
      let directChildren = nub (toList t)
          nestedChildren = concatMap (dig . get' . NotRoot) directChildren
       in directChildren ++ nestedChildren
    get' = Utils.fromJustOrError "pick" . getR base

-- | Remove redundant type vars.
clean t = pick t Root
