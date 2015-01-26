module Penny.Lincoln.DateTime
  ( Date
  , dateToDay
  , Time(..)
  , DateTime(..)
  , Hours(..)
  , Minutes(..)
  , Seconds(..)
  , Enum60(..)
  , Zone
  , zoneToInt
  , intToZone
  , dateTimeToUTC
  ) where

import qualified Data.Time as T

newtype Date = Date { dateToDay :: T.Day }
  deriving (Eq, Ord, Show)

data Time = Time Hours Minutes Seconds
  deriving (Eq, Ord, Show)

-- | An instant in time.  This is a local time.  The 'Eq' and 'Ord'
-- instances are derived, so they aren't useful for determining
-- whether two 'DateTime' refer to the same instant in time; for that,
-- convert the times to UTC times using 'dateTimeToUTC' and then
-- compare those.
data DateTime = DateTime T.Day Hours Minutes Seconds Zone
  deriving (Eq, Ord, Show)

dateTimeToUTC :: DateTime -> T.UTCTime
dateTimeToUTC (DateTime day hrs (Minutes mins) (Seconds secs) zone)
  = T.localTimeToUTC zn lcl
  where
    zn = T.TimeZone (zoneToInt zone) False ""
    lcl = T.LocalTime day tod
    tod = T.TimeOfDay (fromEnum hrs) (fromEnum mins)
      (fromInteger . fromIntegral . fromEnum $ secs)

data Hours = H0 | H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9
  | H10 | H11 | H12 | H13 | H14 | H15 | H16 | H17 | H18
  | H19 | H20 | H21 | H22 | H23
  deriving (Eq, Ord, Show, Enum, Bounded)

data Enum60 = E0 | E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8 | E9
  | E10 | E11 | E12 | E13 | E14 | E15 | E16 | E17 | E18 | E19
  | E20 | E21 | E22 | E23 | E24 | E25 | E26 | E27 | E28 | E29
  | E30 | E31 | E32 | E33 | E34 | E35 | E36 | E37 | E38 | E39
  | E40 | E41 | E42 | E43 | E44 | E45 | E46 | E47 | E48 | E49
  | E50 | E51 | E52 | E53 | E54 | E55 | E56 | E57 | E58 | E59
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Minutes = Minutes Enum60
  deriving (Eq, Ord, Show)

newtype Seconds = Seconds Enum60
  deriving (Eq, Ord, Show)

newtype Zone = Zone { zoneToInt :: Int }
  deriving (Eq, Ord, Show)

intToZone :: Int -> Maybe Zone
intToZone i
  | i < (-2399) = Nothing
  | i > 2399 = Nothing
  | otherwise = Just . Zone $ i
