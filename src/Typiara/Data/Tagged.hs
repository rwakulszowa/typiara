{-# LANGUAGE MultiParamTypeClasses #-}

module Typiara.Data.Tagged where

-- | String representation of data constructors.
-- To be used in `TypeEnv` comparison. Instances should provide only
-- information that cannot be derived from dynamic sources, i.e. it's ok
-- to include identifiers of type constructors (potentially nested), but not
-- the actual values provided to each constructor.
--
-- See `DeriveDataTypeable` extension for an easy to use `toConstr` function.
class Tagged t where
  tag :: t Int -> String
  fromTag :: String -> [Int] -> Maybe (t Int)
