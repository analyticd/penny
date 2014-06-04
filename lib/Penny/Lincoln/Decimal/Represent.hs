-- | Transform concrete representations into abstract ones.

module Penny.Lincoln.Decimal.Represent where

import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Side
import qualified Penny.Lincoln.Decimal.Represent.Ungrouped as U
import qualified Penny.Lincoln.Decimal.Represent.Grouped as G

-- | Transforms a 'Concrete' to an 'Abstract', using the specified
-- 'RadGroup' for the radix point and grouping character.  Rules for
-- digit grouping:
--
-- * Digits to the left of the radix are grouped only if there are
-- at least five digits to the left of the radix.
--
-- * Digits to the right of the radix are never grouped.

grouped :: RadGroup -> Concrete -> Abstract Side
grouped r c = Abstract (G.grouped c) r

-- | Transforms a 'Concrete' to an 'Abstract', using the specified
-- 'RadGroup' for the radix point (the grouping character will not
-- matter, but you have to specify a 'RadGroup' anyway.)

ungrouped :: RadGroup -> Concrete -> Abstract Side
ungrouped r c = Abstract (U.ungrouped c) r
