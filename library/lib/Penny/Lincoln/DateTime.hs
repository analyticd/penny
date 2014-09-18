module Penny.Lincoln.DateTime where

import Data.Time
import qualified Penny.Lincoln.Hours as Hours
import qualified Penny.Lincoln.Minutes as Minutes
import qualified Penny.Seconds as Seconds
import qualified Penny.TimeZoneOffset as TimeZoneOffset

data T = T
  { day :: Day
  , hours :: Hours.T
  , minutes :: Minutes.T
  , seconds :: Seconds.T
  , timeZone :: TimeZoneOffset.T
  } deriving (Eq, Ord, Show)
