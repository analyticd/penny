module Penny.Core.Walker where

import qualified Penny.Core.Stokely as Stokely
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Pebble as Pebble

-- | Number representations that may be neutral or non-neutral.  The
-- type variable is the type of the radix point and grouping
-- character; see, for example, 'Penny.Core.Anna.RadCom.T' or
-- 'Penny.Core.Anna.RadPer.T'.  Unlike a 'Penny.Core.Stokely.T', a 'T'
-- is always parameterized on a 'Penny.Core.Side.T'.
newtype T r
  = T { toStokely :: Stokely.T r Side.T }
  deriving (Eq, Ord, Show)

toPebble :: T r -> Pebble.T
toPebble = Pebble.T . Stokely.toGravel . toStokely
