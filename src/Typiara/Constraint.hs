module Typiara.Constraint where

import Data.List.NonEmpty (NonEmpty)

newtype ConstraintErr c =
  ConstraintErr
    { conflict :: c
    }
  deriving (Eq, Show)

class Constraint c where
  mergeConstraints :: NonEmpty c -> Either (ConstraintErr c) c
  -- TODO: deriveConstraints c -> [c] to propagate constraints from top to bottom / separate Class? (transitiveConstraint?)
