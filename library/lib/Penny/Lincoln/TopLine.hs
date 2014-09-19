module Penny.Lincoln.TopLine where

import qualified Penny.Lincoln.DateTime as DateTime
import qualified Penny.Lincoln.Memo as Memo
import qualified Penny.Lincoln.Number as Number
import qualified Penny.Lincoln.Flag as Flag
import qualified Penny.Lincoln.Payee as Payee
import qualified Penny.Lincoln.Location as Location
import qualified Penny.Lincoln.Clxn as Clxn
import qualified Penny.Lincoln.Serial as Serial

data T = T
  { dateTime :: DateTime.T
  , memo :: Memo.T
  , number :: Maybe Number.T
  , flag :: Maybe Flag.T
  , payee :: Maybe Payee.T
  , location :: Location.T
  , clxn :: Clxn.T
  , globalSerial :: Serial.T
  , clxnSerial :: Serial.T
  } deriving (Eq, Ord, Show)
