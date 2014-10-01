module Penny.Core.Muddy where

import qualified Penny.Core.Janus as Janus
import qualified Penny.Core.Walker as Walker
import qualified Penny.Core.Pebble as Pebble

-- | Number representations that may be zero or non-zero and may have
-- a radix point and grouping character of either
-- 'Penny.Core.Anna.RadCom.T' or 'Penny.Core.Anna.RadPer.T'.
newtype T = T
  { toJanus :: Janus.T Walker.T }

toPebble :: T -> Pebble.T
toPebble (T (Janus.Comma a)) = Walker.toPebble a
toPebble (T (Janus.Period a)) = Walker.toPebble a


instance Eq T where
  (T x) == (T y) = Janus.isEqual (==) (==) x y

instance Ord T where
  compare (T a) (T b) = Janus.compare compare compare a b

instance Show T where
  show (T a) = Janus.show show show a
