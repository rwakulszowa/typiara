module Typiara.ApplicableConstraint where

import Typiara.Constraint (Constraint)

-- Constraint that supports function application.
class (Constraint c) =>
      ApplicableConstraint c
  -- The type a node must satisfy to invoke function application on it.
  where
  funConstraint :: c
  -- A constraint that represents a lack of constraints.
  -- When used in a tree, it can be used to represent an unconstrained tree of links.
  -- The following relation should hold:
  -- `merge nilConstraint = id`
  nilConstraint :: c
