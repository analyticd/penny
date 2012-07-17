module Penny.Cabin.Meta (VisibleNum, unVisibleNum,
                         visibleNums ) where

import qualified Penny.Lincoln as L

newtype VisibleNum = VisibleNum { unVisibleNum :: L.Serial }
                     deriving (Eq, Show)

visibleNums ::
  (VisibleNum -> a -> b)
  -> [L.Box a]
  -> [L.Box b]
visibleNums f = L.serialItems s' where
  s' ser (L.Box m pf) = L.Box (f (VisibleNum ser) m) pf

