{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Standard fields that are available for the top line and for the
-- posting.  In addition, each top line and posting also has a
-- sequence of trees; see "Penny.Tranche".
module Penny.Fields where

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.OFX (TrnType)
import Data.Text (Text)
import Data.Time (ZonedTime, Day, TimeOfDay)
import qualified Data.Time as Time
import qualified Data.Time.Timelens as Timelens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

-- | Fields in the top line.
data TopLineFields = TopLineFields
  { _zonedTime :: ZonedTime
  , _payee :: Maybe Text
  , _origPayee :: Maybe Text
  -- ^ When processing transactions from a financial institution, you
  -- may wish to use one payee name while retaining the institution's
  -- original payee name.  In that case, store the original payee
  -- here, and your payee in '_payee'.
  } deriving (Show, Generic, PrettyVal)

Lens.makeLenses ''TopLineFields

emptyTopLineFields :: ZonedTime -> TopLineFields
emptyTopLineFields zt = TopLineFields zt Nothing Nothing

-- | Fields in the posting.
data PostingFields = PostingFields
  { _number :: Maybe Integer
  , _flag :: Maybe Text
  , _account :: Seq Text
  , _fitid :: Maybe Text
  , _tags :: Seq Text
  , _uid :: Maybe Text
  , _trnType :: Maybe TrnType
  , _origDay :: Maybe Day
  , _origTime :: Maybe TimeOfDay
  , _origZone :: Maybe Int
  } deriving (Show, Generic, PrettyVal)

-- | 'mempty' is 'Nothing' or 'Seq.empty' as appropriate.  'mappend'
-- takes the last non-'Nothing' value or the last non-empty 'Seq', as
-- appropriate.
instance Monoid PostingFields where
  mempty = PostingFields Nothing Nothing Seq.empty Nothing
    Seq.empty Nothing Nothing Nothing Nothing Nothing
  mappend x y = PostingFields
    { _number = last (_number x) (_number y)
    , _flag = last (_flag x) (_flag y)
    , _account = lastSeq (_account x) (_account y)
    , _fitid = last (_fitid x) (_fitid y)
    , _tags = lastSeq (_tags x) (_tags y)
    , _uid = last (_uid x) (_uid y)
    , _trnType = last (_trnType x) (_trnType y)
    , _origDay = last (_origDay x) (_origDay y)
    , _origTime = last (_origTime x) (_origTime y)
    , _origZone = last (_origZone x) (_origZone y)
    }
    where
      last a = maybe a Just
      lastSeq a b
        | Seq.null b = a
        | otherwise = b

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
