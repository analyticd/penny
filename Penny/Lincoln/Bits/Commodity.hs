module Penny.Lincoln.Bits.Commodity where

import Penny.Lincoln.Groups.TextNonEmpty (
  TextNonEmpty ( TextNonEmpty ) )
import Penny.Lincoln.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ))
import Data.Text ( empty )

newtype Commodity =
  Commodity { unCommodity :: AtLeast1 SubCommodity }
  deriving (Eq, Ord, Show)

newtype SubCommodity =
  SubCommodity { unSubCommidity :: TextNonEmpty }
  deriving (Eq, Ord, Show)

-- | Creates a Commodity whose name is only a single character.
charCommodity :: Char -> Commodity
charCommodity c =
  Commodity (AtLeast1 (SubCommodity (TextNonEmpty c empty)) [])
