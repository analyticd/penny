-- | Perhaps this could be called @moment@, as it aims to identify a
-- moment in time. A DateTime is a combination of a LocalTime from
-- Data.Time and a TimeZoneOffset. Previously a DateTime was simply a
-- ZonedTime from Data.Time but ZonedTime has data that Penny does not
-- need.
module Penny.Lincoln.Bits.DateTime
  ( TimeZoneOffset ( offsetToMins )
  , minsToOffset
  , noOffset
  , Hours ( unHours )
  , intToHours
  , zeroHours
  , Minutes ( unMinutes )
  , intToMinutes
  , zeroMinutes
  , Seconds ( unSeconds )
  , picoToSeconds
  , zeroSeconds
  , midnight
  , DateTime ( .. )
  , toUTC
  , sameInstant
  ) where

import qualified Data.Time as T
import Data.Fixed (Pico)

-- | The number of minutes that this timezone is offset from UTC. Can
-- be positive, negative, or zero.
newtype TimeZoneOffset = TimeZoneOffset { offsetToMins :: Int }
                         deriving (Eq, Ord, Show)

-- | Convert minutes to a time zone offset. I'm having a hard time
-- deciding whether to be liberal or strict in what to accept
-- here. Currently it is somewhat strict in that it will fail if
-- absolute value is greater than 840 minutes; currently the article
-- at http://en.wikipedia.org/wiki/List_of_time_zones_by_UTC_offset
-- says there is no offset greater than 14 hours, or 840 minutes.
minsToOffset :: Int -> Maybe TimeZoneOffset
minsToOffset m = if abs m > 840
                 then Nothing
                 else Just $ TimeZoneOffset m

noOffset :: TimeZoneOffset
noOffset = TimeZoneOffset 0

newtype Hours = Hours { unHours :: Int }
                deriving (Eq, Ord, Show)

newtype Minutes = Minutes { unMinutes :: Int }
                  deriving (Eq, Ord, Show)

newtype Seconds = Seconds { unSeconds :: Pico }
                  deriving (Eq, Ord, Show)

-- | succeeds if 0 <= x < 24
intToHours :: Int -> Maybe Hours
intToHours h =
  if h >= 0 && h < 24 then Just . Hours $ h else Nothing

zeroHours :: Hours
zeroHours = Hours 0

-- | succeeds if 0 <= x < 60
intToMinutes :: Int -> Maybe Minutes
intToMinutes m =
  if m >= 0 && m < 60 then Just . Minutes $ m else Nothing

zeroMinutes :: Minutes
zeroMinutes = Minutes 0

-- | succeeds if 0 <= x < 61 (to allow for leap seconds)
picoToSeconds :: Pico -> Maybe Seconds
picoToSeconds s =
  if s >= 0 && s < 61
  then Just . Seconds $ s
  else Nothing

zeroSeconds :: Seconds
zeroSeconds = Seconds 0

midnight :: (Hours, Minutes, Seconds)
midnight = (zeroHours, zeroMinutes, zeroSeconds)

-- | A DateTime is a UTC time that also remembers the local time from
-- which it was set. The Eq and Ord instances account for the local
-- time; that is, even though two DateTimes might indicate the same
-- instant, Eq will indicate they are different if the time zones are
-- different. To see if two DateTimes are the same instant, use
-- sameInstant.
data DateTime = DateTime
  { day :: T.Day
  , hours :: Hours
  , minutes :: Minutes
  , seconds :: Seconds
  , timeZone :: TimeZoneOffset
  } deriving (Eq, Ord, Show)

toUTC :: DateTime -> T.UTCTime
toUTC dt = T.localTimeToUTC tz lt
  where
    tz = T.minutesToTimeZone . offsetToMins . timeZone $ dt
    tod = T.TimeOfDay (unHours h) (unMinutes m) (unSeconds s)
    DateTime d h m s _ = dt
    lt = T.LocalTime d tod

-- | Are these DateTimes the same instant in time, after adjusting for
-- local timezones?

sameInstant :: DateTime -> DateTime -> Bool
sameInstant t1 t2 = toUTC t1 == toUTC t2

