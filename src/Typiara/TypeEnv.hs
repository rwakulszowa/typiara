{-# LANGUAGE FlexibleContexts #-}

module Typiara.TypeEnv where

import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad.Zip                 (munzip, mzip)
import           Data.Bifunctor                    (bimap, first, second)
import           Data.Data                         (Data, Typeable)
import           Data.Either                       (fromLeft, fromRight, isLeft)
import           Data.Foldable                     (find, foldlM, foldrM,
                                                    toList)
import           Data.Function                     (on)
import           Data.List                         (nub)
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NonEmpty
import qualified Data.Map.Strict                   as Map
import qualified Data.Maybe                        as Maybe
import qualified Data.Set                          as Set
import           Data.Traversable                  (mapAccumL)
import qualified Data.Tree                         as Tree
import           Data.Tuple                        (swap)
import           Text.Read                         (readMaybe)

import           Typiara.Data.Tagged               (Tagged (..))
import           Typiara.Fix
import           Typiara.FT                        (FT (..))
import           Typiara.Typ                       (FTUnifyResult (..),
                                                    Typ (..), UnifyError (..),
                                                    unifyFT)
import qualified Typiara.Utils                     as Utils

-- | Generic storage for type variables.
-- Most operations are defined on `TypeEnv` directly, which is a wrapper of
-- this type. In some circumstances, `TypeEnv` invariants may be violated -
-- those scenarios should operate on `TypeVarMap`.
-- After fixing violations, `TypeEnv` should be reintroduced.
--
-- TLDR: Raw type variable storage. If doing unsafe stuff, fetch raw data,
-- do stuff and wrap it with `TypeEnv` when done.
--
-- TODO: KVKMap for Maps where values are containers of keys, creating loops.
-- Generic stuff to work with such objects.
type TypeVarMap t v = Map.Map v (FT t v)

-- | Traverse the structure from the root, aggregating path seen so far.
-- Bail upon finding the same node id twice in one path.
findCycles :: (Foldable t, Ord v, Eq v) => v -> Map.Map v (t v) -> Maybe [v]
findCycles r m =
  case go [] r of
    Left cycle -> Just cycle
    Right ()   -> Nothing
  where
    get' = Utils.fromJustOrError "findCycles.get'" . (m Map.!?)
    go path v
      | v `elem` path = Left (v : path)
      | otherwise = () <$ sequence ([go (v : path) vc | vc <- toList (get' v)])

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
-- TODO: if it's not allowed for ks and vs to be out of sync, looping through
-- keys should be enough
allVars :: (Ord a, Foldable t) => TypeVarMap t a -> Set.Set a
allVars tvs =
  (Set.fromList . Map.keys) tvs `mappend`
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
fmapTVs f tvs = mapValue <$> mapKeysRejectConflicts f tvs
  where
    mapValue = fmap f
    mapKeysRejectConflicts f = Maybe.fromJust . Utils.mapKeysRejectConflicts f

-- | All data required to fully define a type.
-- Note, that type variable names are local. When merging multiple instances,
-- make sure to generate new names.
--
-- Tree traversal always starts at the `root`. It is an error for `root` not to be present in
-- the map.
--
-- `Num` == `[(root, T Num)]`
-- `Bool -> Bool` == `[(root, F a a), (a, T Bool)]`
data TypeEnv t v =
  TypeEnv
    { tvs  :: TypeVarMap t v
    , root :: v
    }
  deriving (Show, Ord)

-- | Build an instance from shape and constraints.
-- Rejects out of sync inputs (i.e. such that the shape doesn't match constraint keys).
--
-- `Num -> Num` == `(f [a, a], [(f, F), (a, Num)])`
fromTree ::
     (Ord v, Tagged t v, Enum v)
  => Tree.Tree v
  -> Map.Map v String
  -> Either (FromTreeError v) (TypeEnv t v)
fromTree shape constraints = do
  ((), s) <- runWriterT (go shape)
  return (TypeEnv (Map.fromList s) root)
  where
    root = Tree.rootLabel shape
    get v = Utils.maybeError (VarNotFound v) (constraints Map.!? v)
    untag :: (Tagged t a) => String -> [a] -> Either (FromTreeError a) (t a)
    untag t vs = Utils.maybeError (UntagError t vs) (fromTag t vs)
    rejectRoot r
      | r == root = Left BadShape
    rejectRoot a = Right a
    -- | Iterate the tree, reading items in the process and storing processed items in a list.
    -- The list will be later compressed into a map.
    go (Tree.Node v vs) = do
      c <- lift (get v)
      vs' <- lift (traverse rejectRoot . fmap Tree.rootLabel $ vs)
      t <- lift (untag c vs')
      tell [(v, t)]
      mapM_ go vs

data FromTreeError a
  = VarNotFound a
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
  let (diff, shape') = Utils.refresh [toEnum 0 ..] shape
  constraints' <- updateKeys diff constraints
  first FromTreeErr (fromTree shape' constraints')
  where
    updateKeys diff m =
      pure (Map.assocs m) >>= traverse f >>= Utils.mapFromListWithKeyM reject
      where
        f (k, v) =
          case diff Map.!? k of
            (Just k') -> Right (k', v)
            Nothing   -> Left (ShapeConstraintsOutOfSync k)
        reject k _ _ = Left (KeyOverlap k)

data FromEnumTreeError a v
  = KeyOverlap v
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

refreshTypeEnv ::
     (Functor t, Foldable t, Ord v, Enum v) => TypeEnv t v -> TypeEnv t v
refreshTypeEnv (TypeEnv tvs r) =
  let (diff, tvs') = refresh tvs
   in TypeEnv {tvs = tvs', root = diff Map.! r}

refreshVs zero = uncurry mzip . first (snd . Utils.refresh [zero ..]) . munzip

-- | Rebuild a tree.
-- Conversion is lossless - the tree contains information about shape, links
-- and constraints.
shape :: (Ord v, Foldable t, Tagged t v) => TypeEnv t v -> Tree.Tree (v, String)
shape te = Tree.unfoldTree f (root te)
  where
    f v =
      let n = Maybe.fromJust (getR te v)
       in ((v, tag n), toList n)

instance (Ord v, Foldable t, Tagged t v) => Eq (TypeEnv t v) where
  (==) = (==) `on` refreshVs 0 . shape

data UnifyEnvError v
  = KeyNotFound v
  | UnifyError UnifyError
  | Cycle [String]
  deriving (Eq, Show)

-- | Single element instance.
singleton :: (Enum v) => FT t v -> TypeEnv t v
singleton t = TypeEnv (Map.singleton r t) r
  where
    r = toEnum 0

-- | Get the root item.
-- Crashes on failure. A root item should always exist.
getRoot :: (Ord v) => TypeEnv t v -> FT t v
getRoot t = Utils.fromJustOrError "No root" (tvs t Map.!? root t)

-- | Get a non-root item.
get :: (Ord v) => TypeEnv t v -> v -> Maybe (FT t v)
get t k = tvs t Map.!? k

getR t k
  | k == root t = Just (getRoot t)
getR t k = get t k

-- | Lower bound of the type's arity.
-- Note, that if the return type is generic enough, arity may grow.
arity :: (Ord v) => TypeEnv t v -> Int
arity = length . outputs

-- | Traverse the tree, returning consecutive return values.
-- Breaks on the first non-F node.
outputs :: (Ord v) => TypeEnv t v -> [FT t v]
outputs t = go $ getRoot t
  where
    get' = Maybe.fromJust . get t
    go n@(F _ ret) = n : go (get' ret)
    go _           = []

-- | Merge two instances, injecting an id from one item into the other.
-- Left root is left intact.
--
-- This operation is performed when two separate types, represented as
-- `TypeEnv`s are merged, e.g. when we know from one source that a type is both
-- `a -> a -> a` and `Num -> b`.
unifyEnv ::
     (Typ t, Functor t, Foldable t, Ord v, Enum v, Data v, Tagged t v, Eq (t v))
  => v
  -> TypeEnv t v
  -> TypeEnv t v
  -> Either (UnifyEnvError v) (TypeEnv t v)
unifyEnv leftIdToMerge (TypeEnv a aRoot) (TypeEnv b bRoot) =
  let leftTvs = annotateMap Left Left a
      rightTvs = annotateMap Right Right b
      -- ^ Move items from each side to disjoint domains to avoid conflicts.
      -- The operation is performed on a map, not a TypeEnv. Root information has
      -- to be transformed below.
      mergedTVs = leftTvs <> rightTvs
      (mapping, refreshed) = refresh mergedTVs
      maptv x = Utils.fromJustOrError "unifyEnv.maptv" $ mapping Map.!? x
   in unifyVars
        (lazyTypeEnv refreshed)
        (maptv (Left leftIdToMerge), maptv (Right bRoot)) >>=
      reconcile (maptv (Left aRoot))
  where
    annotateMap fk fv m =
      Maybe.fromJust (fmap (fmap fv) <$> Utils.mapKeysRejectConflicts fk m)
    -- ^ Key mapping won't crash as long as the function is an annotation (adds
    -- new data without dropping any).

unifyEnvR a = unifyEnv (root a) a

-- | Lazily resolved type variable mapping to another variable.
-- When replacing a type variable `a` with a variable `b` in the whole environment,
-- one can simply turn `a` into `Link a b`.
newtype Link a =
  Link a
  deriving (Eq, Show)

-- | `TypeVarMap` where values may be links to other keys.
-- Values are containers over `RootOrNotRoot v`, instead of just `v`, because
-- restrictions on the shape are more lax - even though a value pointing to the
-- root will always form a cycle, cycles are allowed in `LazyTypeEnv`.
data LazyTypeEnv t v =
  LazyTypeEnv
    { unLazyTypeEnv :: Map.Map v (Either (Link v) (FT t v))
    -- | Largest used variable id. Stored explicitly for efficient insertion.
    , maxVar        :: v
    }
  deriving (Eq, Show)

lazyTypeEnv :: (Functor t, Ord v) => TypeVarMap t v -> LazyTypeEnv t v
lazyTypeEnv m =
  let tvs = Right <$> m
   in LazyTypeEnv tvs (maximum (Map.keys tvs))

-- | Insert a type under a new id that doesn't occur in the environment yet.
-- Returns the updated map and the generated id.
-- TODO: super inefficient; store max counter instead.
insert :: (Ord v, Enum v) => LazyTypeEnv t v -> FT t v -> (v, LazyTypeEnv t v)
insert (LazyTypeEnv tv maxVar) t =
  let newVar = succ maxVar
      tv' = Map.insert newVar (Right t) tv
   in (newVar, LazyTypeEnv tv' newVar)

-- | Replace value stored under `src` with a link pointing to `dst`.
-- Error if `src` is not found in the map.
-- Value stored under `src` is dropped. The caller is responsible for using any
-- stored information prior to calling this function.
link :: (Ord v) => v -> v -> LazyTypeEnv t v -> LazyTypeEnv t v
link src dst lte = lte {unLazyTypeEnv = Map.alter f src (unLazyTypeEnv lte)}
  where
    f (Just _) = Just (Left (Link dst))

-- | Follow a link until hitting the destination (non-link).
-- Replaces chains, such as `a -> b -> c` with a key of `c`.
follow :: (Ord v) => LazyTypeEnv t v -> Link v -> v
follow (LazyTypeEnv m _) = go mempty
  where
    go seen (Link v)
      | v `elem` seen = error "Follow.Cycle"
    go seen (Link v) =
      case m Map.!? v of
        (Just (Left dst)) -> go (v : seen) dst
            -- The destination is also a link. Use it directly.
        (Just (Right _))  -> v
            -- The destination is not a link. Nothing to simplify here.
        Nothing           -> error "Orphaned link"

replace a b v =
  if v == a
    then b
    else v

-- | Replace all occurences of `a` with `b`.
-- Modifies values, not keys.
substitute :: (Functor f, Ord a) => a -> a -> Map.Map a (f a) -> Map.Map a (f a)
substitute a b = fmap (fmap (replace a b))

-- | Replace references to `Link`s with direct pointers to destinations.
reconcile ::
     (Functor t, Foldable t, Ord v, Tagged t v)
  => v
  -> LazyTypeEnv t v
  -> Either (UnifyEnvError v) (TypeEnv t v)
reconcile root lte@(LazyTypeEnv tvs _) =
  let (links, nonLinks) = partitionEitherMap tvs
      directLinks = (follow lte <$> links)
      -- ^ All links point directly to their final target.
      nonLinks' = fmap (applyDiff directLinks) <$> nonLinks
      -- ^ All values pointing to links are replaced with their destinations.
      (hoistedNonLinks, hoistedRoot) =
        if root `Map.member` nonLinks'
           -- root is not a link - leave it as it is.
          then (nonLinks', root)
          -- Root must've been partitioned into the link map.
          -- Promote the variable it points to the new root.
          else let newRoot = directLinks Map.! root
                in (nonLinks', newRoot)
      -- ^ Root is guaranteed to be present in the non-link map.
   in clean <$> unCycle hoistedNonLinks hoistedRoot
  where
    partitionEitherMap m =
      let (ls, rs) = Map.partition isLeft m
       in ( fromLeft (error "unexpected") <$> ls
          , fromRight (error "unexpected") <$> rs)
    -- | Map `k` through `m`, if found. Noop otherwise.
    applyDiff m k = Map.findWithDefault k k m
    unCycle m r =
      case findCycles r m of
        (Just cycle) -> (Left . Cycle) (tag . (m Map.!) <$> cycle)
        Nothing      -> Right (TypeEnv m r)

-- | Unify two variables in a given environment.
unifyVars ::
     (Ord v, Enum v, Typ t, Data v, Functor t, Tagged t v, Eq (t v))
  => LazyTypeEnv t v
  -> (v, v)
  -> Either (UnifyEnvError v) (LazyTypeEnv t v)
unifyVars ti (x, y) = go (follow' x, follow' y)
  -- Resolve (follow) variables before performing any operations.
  --
  -- Calculation state is shared between pending `pairs` and `ti`. `pairs` may
  -- contain variables as they were known at the time of a given iteration, but
  -- `ti` may have changed since.
  -- All information is preserved, so the variables still exist, but they may
  -- point to new destinations. `follow` them to make sure we're operating on
  -- fresh state.
  --
  -- Not doing this would create a risk of dropping some link information.
  -- `link` is lossy and depends on the caller to have used the value before
  -- the call. Since the value represents the fresh state, and the caller operates on
  -- potentially outdated state (`(x, y)` may have been created many iterations
  -- before), dropping the value would be equivalent to losing some past updates.
  where
    follow' = follow ti . Link
    go (x, y)
      | x == y = Right ti
    -- ^ Each iteration creates a new variable and attempts to merge it with
    -- the old ones. If x == y, we would always queue two new pairs of the same
    -- variable, which would in turn produce another pair, and so on.
    -- Return early to break the loop. Same variables are unified by definition.
    go (x, y) = do
      tx <- canonicalize <$> ti `find` x
      ty <- canonicalize <$> ti `find` y
      (FTUnifyResult ut vs) <- first UnifyError (tx `unifyFT` ty)
      pure ti >>= unifySingleType ut >>= foldUnify vs
      where
        find t k = do
          found <- Utils.maybeError (KeyNotFound k) (unLazyTypeEnv t Map.!? k)
          case found of
            (Right t) -> return t
            -- Find the final destination of `k`.
            -- The link is expected to be shortened by the parent function.
            -- Links are not expected.
        canonicalize = fmap (follow ti . Link)
            -- Follow children, replace them with their destinations.
            -- Due to existence of links, there's many ways to represent the same
            -- canonical value. Following links should reduce all of them to the same
            -- object - without a reduction, it's nearly impossible to compare two
            -- instances.
            -- TODO: consider representing canonical forms on the type level.
            -- TODO: canonicalizing requires plenty of repetitive work. Reconsider the
            -- LazyEnv idea.
        foldUnify pairs ti = foldlM unifyVars ti pairs
        unifySingleType t te =
          let (v, te') = insert te t
           in Right (link x v . link y v $ te')

-- | `TypeEnv` representing a chain of functions.
buildFunEnv :: (Ord v, Enum v) => Int -> TypeEnv t v
buildFunEnv arity =
  let (t:ts) = generateFunctionTypes arity (succ root)
   in TypeEnv (Map.fromList ((root, snd t) : ts)) root
  where
    root = toEnum 0
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
funT :: (Enum v, Ord v) => (v, FT t v) -> (v, FT t v) -> TypeEnv t v
funT (a, tA) (b, tB) = TypeEnv (Map.fromList [(r, F a b), (a, tA), (b, tB)]) r
  where
    r = Maybe.fromJust $ find (\x -> x /= a && x /= b) (toEnum <$> [0 ..])

-- | Same as `funT`, except ids are generated automatically.
funT' tA tB =
  let a = toEnum 0
      b = succ a
   in funT (a, tA) (b, tB)

-- | Traverse the tree, returning n'th return node.
-- Will crash if `arity < n`.
nthFunNode :: (Ord v) => TypeEnv t v -> Int -> FT t v
nthFunNode t n = outputs t !! n

nthArgId t n =
  let (F arg _) = nthFunNode t n
   in arg

-- | Decompose the object into standalone arg and fun pieces.
popArg typeInfo =
  let (F argId retId) = nthFunNode typeInfo 0
      pick' = pick typeInfo
   in (pick' argId, pick' retId)

-- | Pick a minimal subset required to represent a type under `id`.
-- The `id` node becomes the new root.
-- When called on `Root`, the function will shake off any redundant type vars.
pick :: (Foldable t, Ord v) => TypeEnv t v -> v -> TypeEnv t v
pick base id =
  let r = get' id
   in TypeEnv
        ((tvs base `Map.restrictKeys` Set.fromList (dig r)) `mappend`
         Map.singleton id r)
        id
  where
    dig t =
      let directChildren = nub (toList t)
          nestedChildren = concatMap (dig . get') directChildren
       in directChildren ++ nestedChildren
    get' = Utils.fromJustOrError "pick" . getR base

-- | Remove redundant type vars.
clean t = pick t (root t)
