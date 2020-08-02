module Typiara.OneOrTwo where

-- Either a (a, a), except it's not `Either`.
-- `data` rather than `type` to avoid implicit conversions.
data OneOrTwo a
  = One a
  | Two a a
  deriving (Eq, Show)
