module Penny.Lincoln.DateTime
  ( Date
  , fromGregorian
  , c'Date'Day
  , dateToDay
  , Time(..)
  , midnight
  , DateTime(..)
  , ZeroTo59(..)
  , Hours(..)
  , Minutes(..)
  , Seconds(..)
  , Zone(..)
  , utcZone
  , dateTimeToUTC
  ) where

import qualified Data.Time as T
import Penny.Lincoln.Rep.Digits
import Penny.Lincoln.PluMin

newtype Date = Date { dateToDay :: T.Day }
  deriving (Eq, Ord, Show)

fromGregorian :: Integer -> Int -> Int -> Maybe Date
fromGregorian y m d = fmap Date $ T.fromGregorianValid y m d

c'Date'Day :: T.Day -> Date
c'Date'Day = Date

data Time = Time Hours Minutes Seconds
  deriving (Eq, Ord, Show)

midnight :: Time
midnight = Time (H0to19 (Just D1z'0) D9z'0)
  (Minutes (ZeroTo59 (Just D5z'0) D9z'0))
  (Seconds (ZeroTo59 (Just D5z'0) D9z'0))

-- | An instant in time.  This is a local time.  The 'Eq' and 'Ord'
-- instances are derived, so they aren't useful for determining
-- whether two 'DateTime' refer to the same instant in time; for that,
-- convert the times to UTC times using 'dateTimeToUTC' and then
-- compare those.
data DateTime = DateTime Date Time Zone
  deriving (Eq, Ord, Show)

dateTimeToUTC :: DateTime -> T.UTCTime
dateTimeToUTC (DateTime (Date day) (Time h (Minutes m) (Seconds s)) z)
  = T.localTimeToUTC tz lt
  where
    tz = T.TimeZone (c'Int'Zone z) False ""
    lt = T.LocalTime day tod
    tod = T.TimeOfDay (c'Int'Hours h) (c'Int'ZeroTo59 m)
      (fromIntegral . c'Int'ZeroTo59 $ s)

data Hours
  = H0to19 (Maybe D1z) D9z
  | H20to23 D3z
  deriving (Eq, Ord, Show)

c'Int'Hours :: Hours -> Int
c'Int'Hours h = case h of
  H0to19 mayD1 d9 -> d1 * 10 + digitToInt d9
    where d1 = maybe 0 digitToInt mayD1
  H20to23 d3-> 20 + digitToInt d3

data ZeroTo59 = ZeroTo59 (Maybe D5z) D9z
  deriving (Eq, Ord, Show)

c'Int'ZeroTo59 :: ZeroTo59 -> Int
c'Int'ZeroTo59 (ZeroTo59 mayD5 d9)
  = ((maybe 0 digitToInt mayD5) * 10) + digitToInt d9

newtype Minutes = Minutes ZeroTo59
  deriving (Eq, Ord, Show)

newtype Seconds = Seconds ZeroTo59
  deriving (Eq, Ord, Show)

data Zone = Zone PluMin D2z D3z D9z D9z
  deriving (Eq, Ord, Show)

utcZone :: Zone
utcZone = Zone Plus D2z'0 D3z'0 D9z'0 D9z'0

c'Int'Zone :: Zone -> Int
c'Int'Zone (Zone pm d3 d2 d1 d0)
  = changeSign
  $ places 3 d3
  + places 2 d2
  + places 1 d1
  + places 0 d0
  where
    changeSign = case pm of { Minus -> negate; Plus -> id }
    places np dig = digitToInt dig * 10 ^ (np `asTypeOf` undefined :: Int)

