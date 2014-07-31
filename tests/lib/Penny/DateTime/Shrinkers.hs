module Penny.DateTime.Shrinkers where

import Penny.DateTime hiding (hours, minutes, seconds, day)
import Test.QuickCheck
import Data.Maybe
import Prelude.Shrinkers
import Data.Time.Shrinkers

timeZoneOffset :: TimeZoneOffset -> [TimeZoneOffset]
timeZoneOffset = fmap f . shrink . offsetToMins
  where
    f = fromMaybe (error "could not shrink time zone offset")
      . minsToOffset

hours :: Hours -> [Hours]
hours = fmap f . shrink . unHours
  where
    f = fromMaybe (error "could not shrink hours")
      . intToHours

minutes :: Minutes -> [Minutes]
minutes = fmap f . shrink . unMinutes
  where
    f = fromMaybe (error "could not shrink minutes")
      . intToMinutes

seconds :: Seconds -> [Seconds]
seconds = fmap f . shrink . unSeconds
  where
    f = fromMaybe (error "could not shrink seconds")
      . intToSeconds

dateTime :: DateTime -> [DateTime]
dateTime (DateTime d h m s o) =
  [ DateTime d' h' m' s' o' | (d', h', m', s', o') <-
    tuple5 day hours minutes seconds timeZoneOffset (d, h, m, s, o) ]
