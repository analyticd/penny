module Penny.DateTime where

import Data.Time
import qualified Penny.Hours as Hours
import qualified Penny.Minutes as Minutes
import qualified Penny.Seconds as Seconds
import qualified Penny.TimeZoneOffset as TimeZoneOffset

data T = T
  { day :: Day
  , hours :: Hours.T
  , minutes :: Minutes.T
  , seconds :: Seconds.T
  , timeZone :: TimeZoneOffset.T
  } deriving (Eq, Ord, Show)
