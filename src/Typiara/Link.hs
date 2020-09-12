module Typiara.Link where

import Typiara.UniqueItemSource (UniqueItemSource)

import qualified Typiara.UniqueItemSource as UniqueItemSource
import qualified Typiara.Utils as Utils

newtype Link =
  Link String
  deriving (Eq, Show, Ord)

uniqueLinkSource :: UniqueItemSource Link
uniqueLinkSource = UniqueItemSource.fromList . map Link $ Utils.allStrings
