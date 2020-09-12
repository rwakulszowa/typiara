module TestConstraint where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Typiara.Constraint

data TestConstraint
  = Fail
  | AnyConstraint
  | ConstraintId String
  deriving (Eq, Show, Ord)

instance Constraint TestConstraint where
  mergeConstraints constraints
    | Fail `elem` constraints = Left . ConstraintErr $ Fail
    | otherwise =
      let cs = (/= AnyConstraint) `NonEmpty.filter` constraints
       in case cs
         -- All constraints were `Any`.
                of
            [] -> Right AnyConstraint
            (c:cs) ->
              if all (== c) cs
                then Right c
                else Left . ConstraintErr $ c
