module TestConstraint where

import Data.List.NonEmpty (NonEmpty(..))

import Constraint

data TestConstraint
  = Fail
  | ConstraintId String
  deriving (Eq, Show, Ord)

instance Constraint TestConstraint where
  mergeConstraints constraints
    | Fail `elem` constraints = Left . ConstraintErr $ Fail
    | otherwise =
      let (c :| cs) = constraints
       in if all (== c) cs
            then Right c
            else Left . ConstraintErr $ c
