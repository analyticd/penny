module Penny.Core.DateTime where

import Data.Time
import qualified Penny.Core.Hours as Hours
import qualified Penny.Core.Minutes as Minutes
import qualified Penny.Core.Seconds as Seconds
import qualified Penny.Core.TimeZoneOffset as TimeZoneOffset

data T = T
  { day :: Day
  , hours :: Hours.T
  , minutes :: Minutes.T
  , seconds :: Seconds.T
  , timeZone :: TimeZoneOffset.T
  } deriving (Eq, Ord, Show)
