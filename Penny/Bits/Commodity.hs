module Penny.Bits.Commodity where

import qualified Penny.TextNonEmpty as NE
  
newtype Commodity = Commodity { unCommodity :: NE.TextNonEmpty }
                  deriving (Eq, Ord)

