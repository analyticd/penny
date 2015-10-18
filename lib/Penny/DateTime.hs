{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.DateTime where
{-
  ( Date
  , fromGregorian
  , c'Date'Day
  , dateToDay
  , Time(..)
  , c'TimeOfDay'Time
  , midnight
  , DateTime(..)
  , ZeroTo59(..)
  , Hours(..)
  , Minutes(..)
  , Seconds(..)
  , Zone(..)
  , c'Int'Zone
  , utcZone
  , dateTimeToUTC
  ) where

import qualified Data.Time as T
import qualified Penny.Grammar as Grammar
import Penny.Display

newtype Date = Date { dateToDay :: T.Day }
  deriving (Eq, Ord, Show)

instance Display Date where
  display (Date dt)
    = shows yr
    . ('-':)
    . shows mo
    . ('-':)
    . shows dy
    where
      (yr, mo, dy) = T.toGregorian dt

fromGregorian :: Integer -> Int -> Int -> Maybe Date
fromGregorian y m d = fmap Date $ T.fromGregorianValid y m d

newtype Minutes = Minutes ZeroTo59
  deriving (Eq, Ord, Show)

instance Display Minutes where
  display (Minutes z) = display z

newtype Seconds = Seconds ZeroTo59
  deriving (Eq, Ord, Show)

instance Display Seconds where
  display (Seconds s) = display s

instance Display Zone where
  display (Zone p d1 d2 d3 d4) = display p . display d1 . display d2
    . display d3 . display d4

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

data Time = Time Hours Minutes Seconds
  deriving (Eq, Ord, Show)

c'TimeOfDay'Time :: Time -> T.TimeOfDay
c'TimeOfDay'Time (Time h (Minutes m) (Seconds s))
  = T.TimeOfDay (c'Int'Hours h) (c'Int'ZeroTo59 m)
  (fromIntegral . c'Int'ZeroTo59 $ s)

instance Display Time where
  display (Time hrs mins ss@(Seconds (ZeroTo59 mayD1 d2)))
    = display hrs . (':':) . display mins . case (mayD1, d2) of
      (Nothing, D9z'0) -> id
      _ -> (':':) . display ss

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

instance Display DateTime where
  display (DateTime d t z)
    = display d
    . (' ':)
    . display t
    . (' ':)
    . display z

dateTimeToUTC :: DateTime -> T.UTCTime
dateTimeToUTC (DateTime (Date day) (Time h (Minutes m) (Seconds s)) z)
  = T.localTimeToUTC tz lt
  where
    tz = T.TimeZone (c'Int'Zone z) False ""
    lt = T.LocalTime day tod
    tod = T.TimeOfDay (c'Int'Hours h) (c'Int'ZeroTo59 m)
      (fromIntegral . c'Int'ZeroTo59 $ s)

-}
