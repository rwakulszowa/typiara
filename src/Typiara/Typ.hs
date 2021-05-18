{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Typiara.Typ where

import           Control.Monad.Zip   (munzip, mzip)
import           Data.Bifunctor
import           Data.Foldable       (toList)
import           Data.Function
import           Data.Hashable
import qualified Data.IntMap.Strict  as IM
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Set            as S
import           Data.Traversable    (mapAccumL)
import qualified Data.Tree           as T
import           GHC.Generics
import           Typiara.Data.Tagged (Tagged)
import           Typiara.FT          (FT (..))
import           Typiara.TypDef
import qualified Typiara.TypeEnv     as TE
import qualified Typiara.Utils       as U

-- | Canonical representation of a TypeEnv.
-- Contains the same data as TypeEnv, but reduced to a base representation.
-- Optimized for read only use. For optimal performance, operations on types
-- should be performed in batches on `TypeEnv`s and converted to `Typ` at the very
-- end.
--
-- Variables are sorted topologically to guarantee there's only one way to represent a given type.
-- The first item describes the root.
newtype Typ t =
  Typ
    { tvs :: [(Int, FT t Int)]
    }
  deriving (Generic)

unpack (Typ tvs) = tvs

root :: Typ t -> Int
root = fst . head . tvs

instance (Show (t Int)) => Show (Typ t) where
  show (Typ tvs) = "Typ { tvs = " ++ show tvs ++ " }"

instance (Eq (t Int)) => Eq (Typ t) where
  (==) = (==) `on` unpack

instance (Ord (t Int)) => Ord (Typ t) where
  compare = compare `on` unpack

instance (Hashable (t Int)) => Hashable (Typ t)

newtype TypError =
  KeyNotFound Int
  deriving (Eq, Show, Ord)

-- | Build a canonical instance from raw data.
-- Items are accumulated in order, starting from the root, and later mapped
-- to lowest possible integers.
-- The result drops all information regarding internal indices used by the arguments.
--
-- FIXME: Cycles are not detected.
typ ::
     (Functor t, Foldable t)
  => Int
  -> IM.IntMap (FT t Int)
  -> Either TypError (Typ t)
typ r tvs = Typ . refresh . reverse . fst <$> go r mempty
  where
    get k = maybe (Left (KeyNotFound k)) Right (tvs IM.!? k)
    children :: Foldable t => FT t Int -> [Int]
    children = toList
          -- Already seen node. Early return.
    go k acc@(xs, seen)
      | k `S.member` seen = Right acc
          -- New node. Add to store and traverse its children.
    go k acc = do
      n <- get k
      travgo (insert k n acc) (children n)
          -- Go from left to right, depth first.
          -- Not `traverse`, because it goes width first.
    travgo init []     = Right init
    travgo init (x:xs) = go x init >>= (`travgo` xs)
    insert k v (xs, seen) = ((k, v) : xs, S.insert k seen)
    refresh xs =
      let (as, bs) = munzip xs
          (diff, as') = U.irefresh as
          bs' = fmap (diff IM.!) <$> bs
       in mzip as' bs'

fromTypeEnv (TE.TypeEnv tvs root) = typ root tvs

intoTypeEnv t = TE.TypeEnv {TE.tvs = IM.fromList (tvs t), TE.root = root t}

data FromEnumTreeError a
  = FromEnumTreeErr (TE.FromEnumTreeError a)
  | TypErr TypError
  deriving (Eq, Show)

fromEnumTree a b =
  first FromEnumTreeErr (TE.fromEnumTree a b) >>= first TypErr . fromTypeEnv

singleton :: (Functor t, Foldable t) => FT t Int -> Typ t
singleton = U.fromRight . fromTypeEnv . TE.singleton

-- | Function `a -> b`
fun :: (Functor t, Foldable t) => FT t Int -> FT t Int -> Typ t
fun a b = Typ [(0, F 1 2), (1, a), (2, b)]

-- | Function of arity `i`, where each argument is an unlinked Nil.
emptyFun :: (Functor t, Foldable t) => Int -> Typ t
emptyFun = U.fromRight . fromTypeEnv . TE.arityFun

makeFun :: (Functor t, Foldable t) => [[Int]] -> Typ t
makeFun = U.fromRight . fromTypeEnv . TE.makeFun

-- | Merge two types together.
-- This operation tells if there exists a set of types satisfying both types.
-- `Left` if the two types are incompatible.
merge ::
     (Functor t, Foldable t, TypDef t, Tagged t, Eq (t Int))
  => Typ t
  -> Typ t
  -> Either MergeError (Typ t)
merge x y = fromTypeEnv' <$> TE.unifyEnvR (intoTypeEnv x) (intoTypeEnv y)
  where
    fromTypeEnv' =
      either (\e -> error $ "merge.fromTypeEnv" ++ show e) id . fromTypeEnv

type MergeError = TE.UnifyEnvError

--
-- | Utils for reading derived properties of an instance.
--
decompose ::
     (Ord a, Enum a, Tagged t, Foldable t)
  => Typ t
  -> (T.Tree a, M.Map a String)
decompose = TE.decompose . intoTypeEnv

outputs :: Typ t -> [FT t Int]
outputs = TE.outputs . intoTypeEnv

arity :: Typ t -> Int
arity = length . outputs
