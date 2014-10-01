module Penny.Core.Philly where

import qualified Penny.Core.Janus as Janus
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Pebble as Pebble
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Core.CoeffExp as CoeffExp
import qualified Penny.Core.Polarity as Polarity

-- | Representations that may have either a period or comma radix, and
-- are non-zero.
data T = T
  { toJanus :: Janus.T Brim.T }

instance Eq T where
  (T x) == (T y) = Janus.isEqual (==) (==) x y

instance Ord T where
  compare (T a) (T b) = Janus.compare compare compare a b

instance Show T where
  show (T a) = Janus.show show show a

toPebble :: Side.T -> T -> Pebble.T
toPebble s (T (Janus.Comma brim)) =
  Pebble.T . Gravel.T ex . Polarity.OffCenter nd $ s
  where
    CoeffExp.T nd ex = Brim.toCoeffExp brim
toPebble s (T (Janus.Period brim)) =
  Pebble.T . Gravel.T ex . Polarity.OffCenter nd $ s
  where
    CoeffExp.T nd ex = Brim.toCoeffExp brim
