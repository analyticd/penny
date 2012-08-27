-- | Metadata that is specific to Cabin.
module Penny.Cabin.Meta (VisibleNum, unVisibleNum,
                         visibleNums ) where

import qualified Penny.Lincoln as L

-- | Each row that is visible on screen is assigned a VisibleNum. This
-- is used to number the rows in the report for the user's benefit. It
-- is also used to determine whether the row is even or odd for the
-- purpose of assigning the background color (this way the background
-- colors can alternate, like a checkbook register.)
newtype VisibleNum = VisibleNum { unVisibleNum :: L.Serial }
                     deriving (Eq, Show)

-- | Assignes VisibleNum to a list of boxes.
visibleNums ::
  (VisibleNum -> a -> b)
  -> [L.Box a]
  -> [L.Box b]
visibleNums f = L.serialItems s' where
  s' ser (L.Box m pf) = L.Box (f (VisibleNum ser) m) pf

