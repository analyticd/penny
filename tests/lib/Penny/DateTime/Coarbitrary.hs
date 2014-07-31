module Penny.DateTime.Coarbitrary where

import Penny.DateTime hiding (day, hours, minutes, seconds)
import Test.QuickCheck
import Data.Time.Coarbitrary

timeZoneOffset :: TimeZoneOffset -> Gen b -> Gen b
timeZoneOffset z = variant $ offsetToMins z

hours :: Hours -> Gen b -> Gen b
hours h = variant $ unHours h

minutes :: Minutes -> Gen b -> Gen b
minutes m = variant $ unMinutes m

seconds :: Seconds -> Gen b -> Gen b
seconds s = variant $ unSeconds s

dateTime :: DateTime -> Gen b -> Gen b
dateTime (DateTime d h m s o) =
  day d . hours h . minutes m . seconds s . timeZoneOffset o
