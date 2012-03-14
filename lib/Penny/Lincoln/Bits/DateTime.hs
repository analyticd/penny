-- | Perhaps this could be called @moment@, as it aims to identify a
-- moment in time. Internally, times are kept as zoned times--for more
-- information on what this means, consult "Data.Time.LocalTime". To
-- create a DateTime, you must supply a zoned time; the easiest way to
-- do this is to use the ZonedTime constructor by building the
-- necessary component parts.
module Penny.Lincoln.Bits.DateTime where

import qualified Data.Time as T
import Data.Monoid (mappend)

newtype DateTime = DateTime { unDateTime :: T.ZonedTime }
                   deriving (Show)

instance Eq DateTime where
  (DateTime dt1) == (DateTime dt2) =
    (T.zonedTimeToLocalTime dt1 == T.zonedTimeToLocalTime dt2)
    &&
    (T.zonedTimeZone dt1) == (T.zonedTimeZone dt2)

instance Ord DateTime where
  compare (DateTime dt1) (DateTime dt2) = mappend c1 c2 where
    c1 = compare (T.zonedTimeToLocalTime dt1)
         (T.zonedTimeToLocalTime dt1)
    c2 = compare (T.zonedTimeZone dt1) (T.zonedTimeZone dt2)
