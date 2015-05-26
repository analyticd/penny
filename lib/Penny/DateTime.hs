{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Penny.DateTime
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
import Penny.Representation
import Penny.PluMin
import Penny.Display
import Penny.Semantic
import Data.Ord

newtype Date = Date { dateToDay :: T.Day }
  deriving (Eq, Ord, Show, SemanticEq, SemanticOrd)

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

c'Date'Day :: T.Day -> Date
c'Date'Day = Date

data Hours
  = H0to19 (Maybe D1z) D9z
  | H20to23 D3z
  deriving (Eq, Ord, Show)

instance Display Hours where
  display (H0to19 Nothing d9) = display Zero . display d9
  display (H0to19 (Just d1) d9) = display d1 . display d9
  display (H20to23 d3) = display Two . display d3

c'Int'Hours :: Hours -> Int
c'Int'Hours h = case h of
  H0to19 mayD1 d9 -> d1 * 10 + digitToInt d9
    where d1 = maybe 0 digitToInt mayD1
  H20to23 d3-> 20 + digitToInt d3

instance SemanticEq Hours where
  x ==@ y = c'Int'Hours x == c'Int'Hours y

instance SemanticOrd Hours where
  compareSemantic = comparing c'Int'Hours

data ZeroTo59 = ZeroTo59 (Maybe D5z) D9z
  deriving (Eq, Ord, Show)

instance Display ZeroTo59 where
  display (ZeroTo59 Nothing d9) = display Zero . display d9
  display (ZeroTo59 (Just d5) d9) = display d5 . display d9

c'Int'ZeroTo59 :: ZeroTo59 -> Int
c'Int'ZeroTo59 (ZeroTo59 mayD5 d9)
  = ((maybe 0 digitToInt mayD5) * 10) + digitToInt d9

instance SemanticEq ZeroTo59 where
  x ==@ y = c'Int'ZeroTo59 x == c'Int'ZeroTo59 y

instance SemanticOrd ZeroTo59 where
  compareSemantic = comparing c'Int'ZeroTo59

newtype Minutes = Minutes ZeroTo59
  deriving (Eq, Ord, Show, SemanticEq, SemanticOrd)

instance Display Minutes where
  display (Minutes z) = display z

newtype Seconds = Seconds ZeroTo59
  deriving (Eq, Ord, Show, SemanticEq, SemanticOrd)

instance Display Seconds where
  display (Seconds s) = display s

data Zone = Zone PluMin D2z D3z D9z D9z
  deriving (Eq, Ord, Show)

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

instance SemanticEq Zone where
  x ==@ y = c'Int'Zone x == c'Int'Zone y

instance SemanticOrd Zone where
  compareSemantic = comparing c'Int'Zone

data Time = Time Hours Minutes Seconds
  deriving (Eq, Ord, Show)

instance SemanticEq Time where
  (Time xh (Minutes xm) (Seconds xs)) ==@
    (Time yh (Minutes ym) (Seconds ys))
      = (c'Int'Hours xh, c'Int'ZeroTo59 xm, c'Int'ZeroTo59 xs) ==
        (c'Int'Hours yh, c'Int'ZeroTo59 ym, c'Int'ZeroTo59 ys)

instance SemanticOrd Time where
  compareSemantic (Time xh (Minutes xm) (Seconds xs))
                  (Time yh (Minutes ym) (Seconds ys))
    = compare
      (c'Int'Hours xh, c'Int'ZeroTo59 xm, c'Int'ZeroTo59 xs)
      (c'Int'Hours yh, c'Int'ZeroTo59 ym, c'Int'ZeroTo59 ys)

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

dateTimeToUTC :: DateTime -> T.UTCTime
dateTimeToUTC (DateTime (Date day) (Time h (Minutes m) (Seconds s)) z)
  = T.localTimeToUTC tz lt
  where
    tz = T.TimeZone (c'Int'Zone z) False ""
    lt = T.LocalTime day tod
    tod = T.TimeOfDay (c'Int'Hours h) (c'Int'ZeroTo59 m)
      (fromIntegral . c'Int'ZeroTo59 $ s)

instance SemanticEq DateTime where
  x ==@ y = dateTimeToUTC x == dateTimeToUTC y

instance SemanticOrd DateTime where
  compareSemantic = comparing dateTimeToUTC
