-- | Perhaps this could be called @moment@, as it aims to identify a
-- moment in time. A DateTime is a combination of a LocalTime from
-- Data.Time and a TimeZoneOffset. Previously a DateTime was simply a
-- ZonedTime from Data.Time but ZonedTime has data that Penny does not
-- need.
module Penny.Lincoln.Bits.DateTime where

import qualified Data.Time as T
import Data.Monoid (mappend)

-- | The number of minutes that this timezone is offset from UTC. Can
-- be positive, negative, or zero.
newtype TimeZoneOffset = TimeZoneOffset { unTimeZoneOffset :: Int }
                         deriving (Eq, Ord, Show)

-- | A DateTime is both a LocalTime and a time zone offset.
data DateTime = DateTime { localTime :: T.LocalTime
                         , timeZone :: TimeZoneOffset }
                   deriving (Eq, Ord, Show)
