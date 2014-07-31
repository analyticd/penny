module Penny.DateTime.Generators where

import Test.QuickCheck
import Penny.DateTime hiding (hours, minutes, seconds)
import Data.Maybe
import Control.Monad
import qualified Data.Time.Generators as G

hours :: Gen Hours
hours = fmap f $ choose (0, 23)
  where
    f = fromMaybe (error "could not generate hours")
      . intToHours

minutes :: Gen Minutes
minutes = fmap f $ choose (0, 59)
  where
    f = fromMaybe (error "could not generate minutes")
      . intToMinutes

seconds :: Gen Seconds
seconds = fmap f $ choose (0, 60)
  where
    f = fromMaybe (error "could not generate seconds")
      . intToSeconds

timeZoneOffset :: Gen TimeZoneOffset
timeZoneOffset = fmap f $ choose (0, 840)
  where
    f = fromMaybe (error "could not generate time zone offset")
      . minsToOffset

dateTime :: Gen DateTime
dateTime = liftM5 DateTime G.day hours minutes seconds timeZoneOffset
