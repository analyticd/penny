module Penny.Lincoln.Bits.Commodity where

import Penny.Lincoln.TextNonEmpty (
  TextNonEmpty ( TextNonEmpty ) )
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text ( empty )

newtype Commodity =
  Commodity { unCommodity :: NonEmpty SubCommodity }
  deriving (Eq, Ord, Show)

newtype SubCommodity =
  SubCommodity { unSubCommodity :: TextNonEmpty }
  deriving (Eq, Ord, Show)

-- | Creates a Commodity whose name is only a single character.
charCommodity :: Char -> Commodity
charCommodity c =
  Commodity ((SubCommodity (TextNonEmpty c empty)) :| [])
