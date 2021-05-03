{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Typiara.SampleTyp
  ( SampleTyp(..)
  ) where

import           Data.Data           (Data, Typeable, toConstr)
import           Typiara.Data.Tagged (Tagged (..))
import           Typiara.FT          (FT (..))
import           Typiara.TypDef      (TypDef (..), UnifyError (..),
                                      UnifyResult (..))

-- | Sample implementation.
data SampleTyp a
  = Seq a
  | Bool
  | Num
  | Str
  deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable, Data, Typeable)

instance TypDef SampleTyp where
  unify (Seq a) (Seq b) = Right (UnifyResult (Seq a) [(a, b)])
  unify x y =
    if x == y
      then Right (UnifyResult x [])
      else Left (ConflictingTypes (tag x) (tag y))

instance Tagged SampleTyp where
  tag = show . toConstr
  -- TODO: try to reuse the magic `gunfold` function from `Data.Data`.
  fromTag "Bool" [] = Just Bool
  fromTag "Num" []  = Just Num
  fromTag "Str" []  = Just Str
  fromTag "Seq" [a] = Just (Seq a)
  fromTag _ _       = Nothing
