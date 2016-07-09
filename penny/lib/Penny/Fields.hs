{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Standard fields that are available for the top line and for the
-- posting.  In addition, each top line and posting also has a
-- sequence of trees; see "Penny.Tranche".
module Penny.Fields where

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Data.Time.Timelens as Timelens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

-- | Fields in the top line.
data TopLineFields = TopLineFields
  { _zonedTime :: ZonedTime
  , _payee :: Maybe Text
  } deriving (Show, Generic)

instance PrettyVal TopLineFields

Lens.makeLenses ''TopLineFields

emptyTopLineFields :: ZonedTime -> TopLineFields
emptyTopLineFields zt = TopLineFields zt Nothing

-- | Fields in the posting.
data PostingFields = PostingFields
  { _number :: Maybe Integer
  , _flag :: Maybe Text
  , _account :: Seq Text
  , _fitid :: Maybe Text
  , _tags :: Seq Text
  , _uid :: Maybe Text
  } deriving (Show, Generic)

instance PrettyVal PostingFields

emptyPostingFields :: PostingFields
emptyPostingFields = PostingFields Nothing Nothing (Seq.empty) Nothing
  Seq.empty Nothing

Lens.makeLenses ''PostingFields

-- | The date of a 'TopLineFields'.
day :: Lens.Lens' TopLineFields Time.Day
day = zonedTime . Timelens.zonedTimeToLocalTime
  . Timelens.localDay

-- | The time of day of a 'TopLineFields'.
timeOfDay :: Lens.Lens' TopLineFields Time.TimeOfDay
timeOfDay = zonedTime . Timelens.zonedTimeToLocalTime . Timelens.localTimeOfDay

timeZone :: Lens.Lens' TopLineFields Time.TimeZone
timeZone = zonedTime . Timelens.zonedTimeZone

timeZoneMinutes :: Lens.Lens' TopLineFields Int
timeZoneMinutes = timeZone . Timelens.timeZoneMinutes

-- | 'True' if the '_flag' exists and has a value of @R@.
reconciled :: PostingFields -> Bool
reconciled = (== "R") . Lens.view (flag . Lens._Just)

-- | 'True' if the '_flag' exists and has a value of @C@.
cleared :: PostingFields -> Bool
cleared = (== "C") . Lens.view (flag . Lens._Just)
