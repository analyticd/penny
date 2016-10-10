{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Standard fields that are available for the top line and for the
-- posting.  In addition, each top line and posting also has a
-- sequence of trees; see "Penny.Tranche".
module Penny.Fields where

import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Semigroup ((<>))
import Data.OFX (TrnType)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import qualified Data.Time.Timelens as Timelens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty

import Penny.Pretty

-- | Fields in the top line.
data TopLineFields = TopLineFields
  { _zonedTime :: ZonedTime
  , _payee :: Text
  , _origPayee :: Text
  -- ^ When processing transactions from a financial institution, you
  -- may wish to use one payee name while retaining the institution's
  -- original payee name.  In that case, store the original payee
  -- here, and your payee in '_payee'.
  } deriving (Show, Generic)

emptyTopLineFields :: ZonedTime -> TopLineFields
emptyTopLineFields zt = TopLineFields zt X.empty X.empty

instance PrettyVal TopLineFields where
  prettyVal (TopLineFields zt pye orig) = Pretty.Rec "TopLineFields"
    [ ("_zonedTime", prettyZonedTime zt)
    , ("_payee", prettyText pye)
    , ("_origPayee", prettyText orig)
    ]

Lens.makeLenses ''TopLineFields

-- | Fields in the posting.
data PostingFields = PostingFields
  { _number :: Maybe Integer
  , _flag :: Text
  , _account :: Seq Text
  , _fitid :: Text
  , _tags :: Seq Text
  , _uid :: Text
  , _trnType :: Maybe TrnType
  , _origDate :: Maybe ZonedTime
  , _memo :: Seq Text
  } deriving (Show, Generic)

instance PrettyVal PostingFields where
  prettyVal x = Pretty.Rec "PostingFields"
    [ ("_number", prettyMaybe Pretty.prettyVal . _number $ x)
    , ("_flag", prettyText . _flag $ x)
    , ("_account", prettySeq prettyText . _account $ x)
    , ("_fitid", prettyText . _fitid $ x)
    , ("_tags", prettySeq prettyText . _tags $ x)
    , ("_uid", prettyText . _uid $ x)
    , ("_trnType", prettyMaybe prettyTrnType . _trnType $ x)
    , ("_origDate", prettyMaybe prettyZonedTime . _origDate $ x)
    ]

-- | 'mempty' is 'Nothing' or 'Seq.empty' as appropriate.  'mappend'
-- takes the last non-'Nothing' value or the last non-empty 'Seq', as
-- appropriate.
instance Monoid PostingFields where
  mempty = PostingFields Nothing X.empty Seq.empty X.empty
    Seq.empty X.empty Nothing Nothing Seq.empty
  mappend x y = PostingFields
    { _number = last (_number x) (_number y)
    , _flag = (_flag x) <> (_flag y)
    , _account = lastSeq (_account x) (_account y)
    , _fitid = (_fitid x) <> (_fitid y)
    , _tags = lastSeq (_tags x) (_tags y)
    , _uid = (_uid x) <> (_uid y)
    , _trnType = last (_trnType x) (_trnType y)
    , _origDate = last (_origDate x) (_origDate y)
    , _memo = lastSeq (_memo x) (_memo y)
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
timeOfDay = zonedTime .
  Timelens.zonedTimeToLocalTime . Timelens.localTimeOfDay

timeZone :: Lens.Lens' TopLineFields Time.TimeZone
timeZone = zonedTime . Timelens.zonedTimeZone

timeZoneMinutes :: Lens.Lens' TopLineFields Int
timeZoneMinutes = timeZone . Timelens.timeZoneMinutes

-- | 'True' if the '_flag' exists and has a value of @R@.
reconciled :: PostingFields -> Bool
reconciled = (== "R") . Lens.view flag

-- | 'True' if the '_flag' exists and has a value of @C@.
cleared :: PostingFields -> Bool
cleared = (== "C") . Lens.view flag
