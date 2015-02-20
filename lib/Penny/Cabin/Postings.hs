-- | The Postings report
module Penny.Cabin.Postings where

import Control.Applicative
import Penny.Lincoln
import Rainbox
import qualified Data.Traversable as T
import qualified Data.Foldable as F

-- | Examines all available Trio to obtain a function that will
-- render a given Qty given its corresponding Commodity.
--
-- Examines every Trio.  If the Trio has both a quantity
-- representation and a commodity, takes note of which radix point was
-- used and whether the digits are grouped.
--
-- The function returned will represent Qty.  For any given Commodity,
-- it uses the radix point and grouping character that is seen most
-- frequently for the given Commodity.  The function will fail if the
-- given Commodity has not been seen.
howToRender
  :: (Applicative l, T.Traversable t1)
  => t1 (Clatch l)
  -> l (Commodity -> Maybe (Qty -> QtyRepAnyRadix))
howToRender = undefined

trioRendering
  :: Trio
  -> Maybe (Either (Radix RadCom, Maybe RadCom)
                   (Radix RadPer, Maybe RadPer))
trioRendering = undefined

newtype VisibleSer = VisibleSer Serset
  deriving (Eq, Ord, Show)

data Tranche l = Tranche (Clatch l) FilteredSer VisibleSer Balances

postingsBox
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l Box
postingsBox cols
  = fmap (gridByRows . F.toList . fmap F.toList)
  . makeRows cols

makeRows
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l (t2 (t1 Cell))
makeRows cols = T.sequenceA . fmap T.sequenceA . fmap mkRow
  where
    mkRow trch = fmap ($ trch) cols
