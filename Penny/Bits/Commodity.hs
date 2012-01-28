module Penny.Bits.Commodity where

import qualified Penny.TextNonEmpty as NE
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ))
import Data.Text ( empty )

newtype Commodity =
  Commodity { unCommodity :: AtLeast1 SubCommodity }
  deriving (Eq, Ord)

newtype SubCommodity =
  SubCommodity { unSubCommidity :: NE.TextNonEmpty }
  deriving (Eq, Ord)

-- | Creates a Commodity whose name is only a single character.
charCommodity :: Char -> Commodity
charCommodity c =
  Commodity (AtLeast1 (SubCommodity (NE.TextNonEmpty c empty)) [])
