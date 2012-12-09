-- | Metadata that is specific to Cabin.
module Penny.Cabin.Meta (VisibleNum, unVisibleNum,
                         visibleNumBoxes, visibleNums ) where

import Control.Applicative ((*>))
import qualified Data.Traversable as Tr
import qualified Penny.Lincoln as L

-- | Each row that is visible on screen is assigned a VisibleNum. This
-- is used to number the rows in the report for the user's benefit. It
-- is also used to determine whether the row is even or odd for the
-- purpose of assigning the background color (this way the background
-- colors can alternate, like a checkbook register.)
newtype VisibleNum = VisibleNum { unVisibleNum :: L.Serial }
                     deriving (Eq, Show)

-- | Assigns VisibleNum to a list of boxes.
visibleNumBoxes ::
  (VisibleNum -> a -> b)
  -> [L.Box a]
  -> [L.Box b]
visibleNumBoxes f bs = L.makeSerials k
  where
    k = Tr.sequenceA (replicate (length bs) L.incrementBack)
        *> mapM assign bs
    assign (L.Box m pf) = fmap g L.getSerial
      where
        g ser = L.Box (f (VisibleNum ser) m) pf


-- | Assigns VisibleNum to a list.
visibleNums :: (VisibleNum -> a -> b) -> [a] -> [b]
visibleNums f as = L.makeSerials k
  where
    k = Tr.sequenceA (replicate (length as) L.incrementBack)
        *> mapM assign as
    assign a = fmap (\ser -> f (VisibleNum ser) a) L.getSerial

