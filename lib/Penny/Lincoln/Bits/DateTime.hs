{-# LANGUAGE DeriveGeneric, CPP #-}

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
  , intToSeconds
  , zeroSeconds
  , midnight
  , DateTime ( .. )
  , dateTimeMidnightUTC
  , toUTC
  , toZonedTime
  , fromZonedTime
  , sameInstant
  , showDateTime
  ) where

import qualified Control.Monad as M
import qualified Data.Time as T
import qualified Data.Binary as B
import Data.Binary (get, put)
import GHC.Generics (Generic)

#ifdef test
import Control.Monad (liftM5)
import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary, arbitrary)
#endif

-- | The number of minutes that this timezone is offset from UTC. Can
-- be positive, negative, or zero.
newtype TimeZoneOffset = TimeZoneOffset { offsetToMins :: Int }
                         deriving (Eq, Ord, Show, Generic)

instance B.Binary TimeZoneOffset

#ifdef test
instance Arbitrary TimeZoneOffset where
  arbitrary = fmap TimeZoneOffset (QC.choose (-840, 840))
#endif

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
                deriving (Eq, Ord, Show, Generic)

instance B.Binary Hours

newtype Minutes = Minutes { unMinutes :: Int }
                  deriving (Eq, Ord, Show, Generic)

instance B.Binary Minutes

newtype Seconds = Seconds { unSeconds :: Int }
                  deriving (Eq, Ord, Show, Generic)

instance B.Binary Seconds

#ifdef test
instance Arbitrary Hours where
  arbitrary = fmap Hours $ QC.choose (0, 23)

instance Arbitrary Minutes where
  arbitrary = fmap Minutes $ QC.choose (0, 59)

instance Arbitrary Seconds where
  arbitrary = fmap Seconds $ QC.choose (0, 60)
#endif

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
intToSeconds :: Int -> Maybe Seconds
intToSeconds s =
  if s >= 0 && s < 61
  then Just . Seconds $ s
  else Nothing

zeroSeconds :: Seconds
zeroSeconds = Seconds 0

midnight :: (Hours, Minutes, Seconds)
midnight = (zeroHours, zeroMinutes, zeroSeconds)

-- | A DateTime is a a local date and time, along with a time zone
-- offset.  The Eq and Ord instances are derived; therefore, two
-- DateTime instances will not be equivalent if the time zone offsets
-- are different, even if they are the same instant. To compare one
-- DateTime to another, you probably want to use 'toUTC' and compare
-- those. To see if two DateTime are the same instant, use
-- 'sameInstant'.
data DateTime = DateTime
  { day :: T.Day
  , hours :: Hours
  , minutes :: Minutes
  , seconds :: Seconds
  , timeZone :: TimeZoneOffset
  } deriving (Eq, Ord, Show)

#ifdef test
instance Arbitrary DateTime where
  arbitrary = liftM5 DateTime
    ( fmap (T.ModifiedJulianDay . fromIntegral)
      $ QC.suchThat QC.arbitraryBoundedIntegral (> (0 :: Int)))
    arbitrary arbitrary arbitrary arbitrary
#endif

instance B.Binary DateTime where
  get = M.liftM5 DateTime (fmap T.ModifiedJulianDay B.get)
                 get get get get
  put (DateTime d h m s t) =
    put (T.toModifiedJulianDay d) >> put h >> put m >> put s >> put t

dateTimeMidnightUTC :: T.Day -> DateTime
dateTimeMidnightUTC d = DateTime d h m s z
  where
    (h, m, s) = midnight
    z = noOffset

toZonedTime :: DateTime -> T.ZonedTime
toZonedTime dt = T.ZonedTime lt tz
  where
    d = day dt
    lt = T.LocalTime d tod
    tod = T.TimeOfDay (unHours . hours $ dt) (unMinutes . minutes $ dt)
          (fromIntegral . unSeconds . seconds $ dt)
    tz = T.TimeZone (offsetToMins . timeZone $ dt) False ""

fromZonedTime :: T.ZonedTime -> Maybe DateTime
fromZonedTime (T.ZonedTime (T.LocalTime d tod) tz) = do
  h <- intToHours . T.todHour $ tod
  m <- intToMinutes . T.todMin $ tod
  let (sWhole, _) = properFraction . T.todSec $ tod
  s <- intToSeconds sWhole
  tzo <- minsToOffset . T.timeZoneMinutes $ tz
  return $ DateTime d h m s tzo

toUTC :: DateTime -> T.UTCTime
toUTC dt = T.localTimeToUTC tz lt
  where
    tz = T.minutesToTimeZone . offsetToMins . timeZone $ dt
    tod = T.TimeOfDay (unHours h) (unMinutes m)
          (fromIntegral . unSeconds $ s)
    DateTime d h m s _ = dt
    lt = T.LocalTime d tod

-- | Are these DateTimes the same instant in time, after adjusting for
-- local timezones?

sameInstant :: DateTime -> DateTime -> Bool
sameInstant t1 t2 = toUTC t1 == toUTC t2

-- | Shows a DateTime in a pretty way.
showDateTime :: DateTime -> String
showDateTime (DateTime d h m s tz) =
  ds ++ " " ++ hmss ++ " " ++ showOffset
  where
    ds = show d
    hmss = hs ++ ":" ++ ms ++ ":" ++ ss
    hs = pad0 . show . unHours $ h
    ms = pad0 . show . unMinutes $ m
    ss = pad0 . show . unSeconds $ s
    pad0 str = if length str < 2 then '0':str else str
    showOffset =
      let (zoneHr, zoneMin) = abs (offsetToMins tz) `divMod` 60
          sign = if offsetToMins tz < 0 then "-" else "+"
      in sign ++ pad0 (show zoneHr) ++ pad0 (show zoneMin)

