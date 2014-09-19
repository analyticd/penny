module Penny.Lincoln.Posting where

import qualified Penny.Memo as Memo
import qualified Penny.Number as Number
import qualified Penny.Lincoln.Flag as Flag
import qualified Penny.Lincoln.Payee as Payee
import qualified Penny.Tags as Tags
import qualified Penny.Lincoln.Account as Account
import qualified Penny.Location as Location
import qualified Penny.Serial as Serial
import qualified Penny.Lincoln.Trio as Trio

data T = T
  { memo :: Memo.T
  , number :: Maybe Number.T
  , flag :: Maybe Flag.T
  , payee :: Maybe Payee.T
  , tags :: Tags.T
  , account :: Account.T
  , location :: Location.T
  , globalSerial :: Serial.T
  , clxnSerial :: Serial.T
  , trio :: Trio.T
  } deriving (Eq, Ord, Show)
