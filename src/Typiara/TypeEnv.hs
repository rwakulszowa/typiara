{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
import qualified Data.IntMap.Strict                as IM
import qualified Data.IntSet                       as IS
import           Data.List                         (mapAccumR, nub)
import           Data.List.NonEmpty                (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                as NonEmpty
import qualified Data.Map.Strict                   as M
import qualified Data.Maybe                        as Maybe
import qualified Data.Set                          as Set
import qualified Data.Set.Ordered                  as OSet
import           Data.Traversable                  (mapAccumL)
import qualified Data.Tree                         as Tree
import           Data.Tuple                        (swap)
import           Text.Read                         (readMaybe)

import           Typiara.Data.Tagged               (Tagged (..))
import           Typiara.FT                        (FT (..), FTUnifyResult (..),
                                                    unifyFT)
import           Typiara.Fix
import           Typiara.TypDef                    (TypDef (..),
                                                    UnifyError (..))
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
type TypeVarMap t = IM.IntMap (FT t Int)

-- | Traverse the structure from the root, aggregating path seen so far.
-- Bail upon finding the same node id twice in one path.
findCycles :: (Foldable t) => Int -> IM.IntMap (t Int) -> Maybe [Int]
findCycles r m =
  case go OSet.empty r of
    Left cycle -> Just cycle
    Right ()   -> Nothing
  where
    get' = Utils.fromJustOrError "findCycles.get'" . (m IM.!?)
    go path v
      | v `OSet.member` path = Left (v : toList path)
      | otherwise =
        () <$ sequence ([go (v OSet.|< path) vc | vc <- toList (get' v)])

-- | Map `a`s to `b`s.
-- `b` *must* generate unique values on each `succ` call.
refresh ::
     (Functor t, Foldable t) => TypeVarMap t -> (IM.IntMap Int, TypeVarMap t)
refresh t =
  let mapping = IM.fromList $ Set.elems (allVars t) `zip` [0 ..]
   in (mapping, fmapTVs (mapping IM.!) t)

-- | Dig all stored type variable names.
-- TODO: if it's not allowed for ks and vs to be out of sync, looping through
-- keys should be enough
allVars :: (Foldable t) => TypeVarMap t -> Set.Set Int
allVars tvs =
  (Set.fromList . IM.keys) tvs `mappend`
  (Set.fromList . concatMap toList . IM.elems) tvs

-- | Note on pseudo instances.
-- The type doesn't satisfy `fmap` because it uses `a` twice in its signature.
-- There's also an additional requirement on fmap's `f`, namely, that it cannot
-- cause any conflicts.
-- As a result, the type is not a proper `Functor`.
--
-- | `fmap` substitute.
-- `f` *must not* cause conflicts.
fmapTVs :: (Functor t) => (Int -> Int) -> TypeVarMap t -> TypeVarMap t
fmapTVs f tvs = mapValue <$> mapks f tvs
  where
    mapValue = fmap f
    mapks f = Maybe.fromJust . Utils.mapIKeysRejectConflicts f

-- | All data required to fully define a type.
-- Note, that type variable names are local. When merging multiple instances,
-- make sure to generate new names.
--
-- Tree traversal always starts at the `root`. It is an error for `root` not to be present in
-- the map.
--
-- `Num` == `[(root, T Num)]`
-- `Bool -> Bool` == `[(root, F a a), (a, T Bool)]`
data TypeEnv t =
  TypeEnv
    { tvs  :: TypeVarMap t
    , root :: Int
    }

instance (Show (t Int)) => Show (TypeEnv t) where
  show te =
    "TypeEnv { tvs = " ++ show (tvs te) ++ ", root = " ++ show (root te) ++ " }"

-- | Direct comparison of raw data.
-- To compare instances ignoring non-key details (i.e. type var ids), see `Typ`.
instance (TypDef t, Foldable t, Functor t, Tagged t, Eq (t Int)) =>
         Eq (TypeEnv t) where
  (==) = (==) `on` unpack
    where
      unpack (TypeEnv a b) = (a, b)

-- | Build an instance from shape and constraints.
-- Rejects out of sync inputs (i.e. such that the shape doesn't match constraint keys).
--
-- `Num -> Num` == `(f [a, a], [(f, F), (a, Num)])`
fromTree ::
     (Tagged t)
  => Tree.Tree Int
  -> IM.IntMap String
  -> Either FromTreeError (TypeEnv t)
fromTree shape constraints = do
  ((), s) <- runWriterT (go shape)
  return (TypeEnv (IM.fromList s) root)
  where
    root = Tree.rootLabel shape
    get v = Utils.maybeError (VarNotFound v) (constraints IM.!? v)
    untag :: (Tagged t) => String -> [Int] -> Either FromTreeError (t Int)
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

data FromTreeError
  = VarNotFound Int
  | UntagError String [Int]
  | BadShape
  -- ^ Thrown if `Root` appears in a non-root position.
  deriving (Eq, Show)

-- | A more convenient variant of `fromTree` that handles any enum ids.
fromEnumTree ::
     (Ord a, Enum a, Tagged t)
  => Tree.Tree a
  -> M.Map a String
  -> Either (FromEnumTreeError a) (TypeEnv t)
fromEnumTree shape constraints = do
  let (diff, shape') = Utils.refresh [toEnum 0 ..] shape
  constraints' <- updateKeys diff constraints
  first FromTreeErr (fromTree shape' (fromRegularMap constraints'))
  where
    fromRegularMap = IM.fromList . M.assocs
    updateKeys diff m =
      pure (M.assocs m) >>= traverse f >>= Utils.mapFromListWithKeyM reject
      where
        f (k, v) =
          case diff M.!? k of
            (Just k') -> Right (k', v)
            Nothing   -> Left (ShapeConstraintsOutOfSync k)
        reject k _ _ = Left (KeyOverlap k)

data FromEnumTreeError a
  = KeyOverlap Int
  -- ^ Happens only on bad implementations of `Enum`.
  | ShapeConstraintsOutOfSync a
  | FromTreeErr FromTreeError
  deriving (Eq, Show)

-- | Reverse of `fromEnumTree`.
decompose ::
     (Ord a, Enum a, Tagged t, Foldable t)
  => TypeEnv t
  -> (Tree.Tree a, M.Map a String)
decompose = stripTags . refresh' . shape
  where
    refresh' = Utils.refreshVs (toEnum 0)
    stripTags = swap . mapAccumL f mempty
      where
        f s (v, tag) =
          let s' =
                case s M.!? v of
                  Nothing -> M.insert v tag s
                  (Just tag')
                    | tag' == tag -> s
                  (Just tag') -> error "Tags out of sync"
           in (s', v)

-- | Rebuild a tree.
-- Conversion is lossless - the tree contains information about shape, links
-- and constraints.
shape :: (Foldable t, Tagged t) => TypeEnv t -> Tree.Tree (Int, String)
shape te = Utils.refreshVs 0 $ Tree.unfoldTree f (root te)
  where
    f v =
      let n = Maybe.fromJust (getR te v)
       in ((v, tag n), toList n)

data UnifyEnvError
  = KeyNotFound Int
  | UnifyError UnifyError
  | Cycle [String]
  deriving (Eq, Show)

-- | Single element instance.
singleton :: FT t Int -> TypeEnv t
singleton t = TypeEnv (IM.singleton r t) r
  where
    r = 0

-- | Get the root item.
-- Crashes on failure. A root item should always exist.
getRoot :: TypeEnv t -> FT t Int
getRoot t = Utils.fromJustOrError "No root" (tvs t IM.!? root t)

-- | Get a non-root item.
get :: TypeEnv t -> Int -> Maybe (FT t Int)
get t k = tvs t IM.!? k

getR t k
  | k == root t = Just (getRoot t)
getR t k = get t k

-- | Lower bound of the type's arity.
-- Note, that if the return type is generic enough, arity may grow.
arity :: TypeEnv t -> Int
arity = length . outputs

-- | Traverse the tree, returning consecutive return values.
-- Breaks on the first non-F node.
outputs :: TypeEnv t -> [FT t Int]
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
--
-- The function performs more expensive transformations on the right argument.
-- When used in a loop, make sure the left argument is used as the accumulator.
unifyEnv ::
     (TypDef t, Functor t, Foldable t, Tagged t, Eq (t Int))
  => Int
  -> TypeEnv t
  -> TypeEnv t
  -> Either UnifyEnvError (TypeEnv t)
unifyEnv aIdToMerge a b =
  let (f, b') = translateBy (maxVar a + 1) b
      -- ^ Shift b to a clean section of the domain to avoid conflicts.
      -- a is left untouched. References to `b` should be mapped through `f`.
      abtvs = tvs a <> tvs b'
   in unifyVars (lazyTypeEnv abtvs) (aIdToMerge, root b') >>= reconcile (root a)
  where
    maxVar = maxKey . tvs
    translateBy x (TypeEnv tvs root) =
      let tvs' = fmap (fmap f) . IM.mapKeysMonotonic f $ tvs
          root' = f root
       in (f, TypeEnv tvs' root')
      where
        f = (+ x)

unifyEnvR a = unifyEnv (root a) a

-- | Lazily resolved type variable mapping to another variable.
-- When replacing a type variable `a` with a variable `b` in the whole environment,
-- one can simply turn `a` into `Link a b`.
newtype Link =
  Link Int
  deriving (Eq, Show)

-- | `TypeVarMap` where values may be links to other keys.
-- Values are containers over `RootOrNotRoot v`, instead of just `v`, because
-- restrictions on the shape are more lax - even though a value pointing to the
-- root will always form a cycle, cycles are allowed in `LazyTypeEnv`.
newtype LazyTypeEnv t =
  LazyTypeEnv
    { unLazyTypeEnv :: IM.IntMap (Either Link (FT t Int))
    }

maxKey = fst . Utils.fromJustOrError "maxKey" . IM.lookupMax

lazyTypeEnv :: (Functor t) => TypeVarMap t -> LazyTypeEnv t
lazyTypeEnv m = LazyTypeEnv (Right <$> m)

-- | Insert a type under a new id that doesn't occur in the environment yet.
-- Returns the updated map and the generated id.
insert :: LazyTypeEnv t -> FT t Int -> (Int, LazyTypeEnv t)
insert lte@(LazyTypeEnv tv) t =
  let newVar = succ (maxKey (unLazyTypeEnv lte))
      tv' = IM.insert newVar (Right t) tv
   in (newVar, LazyTypeEnv tv')

insertMany :: LazyTypeEnv t -> [FT t Int] -> ([Int], LazyTypeEnv t)
insertMany x = swap . mapAccumL f x
  where
    f te t =
      let (v, te') = insert te t
       in (te', v)

-- | Replace value stored under `src` with a link pointing to `dst`.
-- Error if `src` is not found in the map.
-- Value stored under `src` is dropped. The caller is responsible for using any
-- stored information prior to calling this function.
link :: Int -> Int -> LazyTypeEnv t -> LazyTypeEnv t
link src dst lte = lte {unLazyTypeEnv = IM.alter f src (unLazyTypeEnv lte)}
  where
    f (Just _) = Just (Left (Link dst))

-- | Follow a link until hitting the destination (non-link).
-- Replaces chains, such as `a -> b -> c` with a key of `c`.
follow :: LazyTypeEnv t -> Link -> Int
follow (LazyTypeEnv m) = go mempty
  where
    go seen (Link v)
      | v `elem` seen = error "Follow.Cycle"
    go seen (Link v) =
      case m IM.!? v of
        (Just (Left dst)) -> go (v : seen) dst
            -- The destination is also a link. Use it directly.
        (Just (Right _))  -> v
            -- The destination is not a link. Nothing to simplify here.
        Nothing           -> error "Orphaned link"

-- | Replace references to `Link`s with direct pointers to destinations.
reconcile ::
     (Functor t, Foldable t, Tagged t)
  => Int
  -> LazyTypeEnv t
  -> Either UnifyEnvError (TypeEnv t)
reconcile root lte =
  let (links, nonLinks) = partitionEitherMap (unLazyTypeEnv lte)
      directLinks = (follow lte <$> links)
      -- ^ All links point directly to their final target.
      nonLinks' = fmap (applyDiff directLinks) <$> nonLinks
      -- ^ All values pointing to links are replaced with their destinations.
      (hoistedNonLinks, hoistedRoot) =
        if root `IM.member` nonLinks'
           -- root is not a link - leave it as it is.
          then (nonLinks', root)
          -- Root must've been partitioned into the link map.
          -- Promote the variable it points to the new root.
          else let newRoot = directLinks IM.! root
                in (nonLinks', newRoot)
      -- ^ Root is guaranteed to be present in the non-link map.
   in clean <$> unCycle hoistedNonLinks hoistedRoot
  where
    partitionEitherMap m =
      let (ls, rs) = IM.partition isLeft m
       in ( fromLeft (error "unexpected") <$> ls
          , fromRight (error "unexpected") <$> rs)
    -- | Map `k` through `m`, if found. Noop otherwise.
    applyDiff m k = IM.findWithDefault k k m
    unCycle m r =
      case findCycles r m of
        (Just cycle) -> (Left . Cycle) (tag . (m IM.!) <$> cycle)
        Nothing      -> Right (TypeEnv m r)

-- | Unify two variables in a given environment.
unifyVars ::
     (TypDef t, Functor t, Tagged t, Eq (t Int))
  => LazyTypeEnv t
  -> (Int, Int)
  -> Either UnifyEnvError (LazyTypeEnv t)
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
      (FTUnifyResult ut vs cs) <- first UnifyError (tx `unifyFT` ty)
      pure ti >>= unifySingleType ut >>= foldUnify vs >>= addConstraints cs
      where
        find t k = do
          found <- Utils.maybeError (KeyNotFound k) (unLazyTypeEnv t IM.!? k)
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
        -- Add new constraints to existing ids, by:
        -- 1. creating fresh nodes bounded by the constraints
        -- 2. linking said nodes with target nodes (provided by the caller)
        addConstraints vcs te =
          let (vs, cs) = munzip vcs
              (vs', te') = insertMany te cs
              vvs' = mzip vs vs'
           in foldUnify vvs' te'

-- | `TypeEnv` representing a chain of unlinked functions.
arityFun :: Int -> TypeEnv t
arityFun arity = makeFun [[0 .. arity]]

-- | Build a function of Fun and Nil nodes, where function shape is provided by the user.
-- The user controls the ids used by the produced nodes - if multiple elements use
-- the same id, the nodes will be linked.
--
-- NOTE: I *think* it's impossible to provide an invalid input (i.e. one that would form
-- an illegal function, e.g. a cycle), because all leaves are Nils and all nodes are Funs.
--
-- For now, functions up to 2 levels deep are allowed. If need be, replace the [[ ]] container with a
-- dedicated, tree-like input.
makeFun :: [[Int]] -> TypeEnv t
makeFun shape =
  let (freeIds', funs) = mapAccumR genOne freeIds shape
      (topFun, freeIds'') = funChain (root <$> funs) freeIds'
   in TypeEnv (IM.fromList (topFun <> mconcat funs)) (root topFun)
  where
    root = fst . head -- By convention.
    freeIds = [i | i <- [0 ..], i `IS.notMember` reservedIds]
      where
        reservedIds = mconcat (IS.fromList <$> shape)
    genOne vs shape =
      let (fun, vs') = genFun shape vs
       in (vs', fun)
    genFun :: [Int] -> [Int] -> ([(Int, FT t Int)], [Int])
    -- Generate function nodes recursively, accumulating a map of nodes in the process.
    -- The first item in returned list of nodes represents the current root.
    genFun [v] freeVs = ([(v, Nil)], freeVs)
    -- ^ Last item - no more functions to make.
    -- Use the user provided id directly.
    --
    -- Build a function node.
    -- Use a user provided id for the argument, and free ids for the remaining nodes.
    genFun (vArg:vs) (vFun:freeVs) =
      let (ret, freeVs') = genFun vs freeVs
          vRet = root ret -- By convention.
          arg = (vArg, Nil)
          node = (vFun, F vArg vRet)
       in (node : arg : ret, freeVs') -- Current root must be the head, by convention.
    funChain :: [Int] -> [Int] -> ([(Int, _)], [Int])
    -- Build a chain of functions, where all leaves are identified by provided ids.
    -- Each newly created node takes a free id.
    -- The result is not a valid function representation by itself. It must me concatenated
    -- with a list including definitions of all provided ids.
    -- It happens to be the same operation as generating a regular function.
    funChain = genFun

-- | Build a function `tA -> tB`, where the argument is stored under `a` and
-- the result under `b`.
funT :: (Int, FT t Int) -> (Int, FT t Int) -> TypeEnv t
funT (a, tA) (b, tB) = TypeEnv (IM.fromList [(r, F a b), (a, tA), (b, tB)]) r
  where
    r = Maybe.fromJust $ find (\x -> x /= a && x /= b) [0 ..]

-- | Same as `funT`, except ids are generated automatically.
funT' tA tB =
  let a = toEnum 0
      b = succ a
   in funT (a, tA) (b, tB)

-- | Traverse the tree, returning n'th return node.
-- Will crash if `arity < n`.
nthFunNode :: TypeEnv t -> Int -> FT t Int
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
pick :: (Foldable t) => TypeEnv t -> Int -> TypeEnv t
pick base id =
  let r = get' id
   in TypeEnv
        ((tvs base `IM.restrictKeys` dig id) `mappend` IM.singleton id r)
        id
  where
    dig v = go mempty v
      where
        go acc v
          | v `IS.member` acc = acc
        go acc v =
          let t = get' v
              children = IS.fromList (toList t)
           in IS.foldl go (IS.insert v acc) children
    get' = Utils.fromJustOrError "pick" . getR base

-- | Remove redundant type vars.
clean t = pick t (root t)
