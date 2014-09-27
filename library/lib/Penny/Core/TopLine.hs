module Penny.Core.TopLine where

import qualified Penny.Core.DateTime as DateTime
import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Number as Number
import qualified Penny.Core.Flag as Flag
import qualified Penny.Core.Payee as Payee
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local

data T = T
  { dateTime :: DateTime.T
  , memo :: Memo.T
  , number :: Maybe Number.T
  , flag :: Maybe Flag.T
  , payee :: Maybe Payee.T
  , location :: Location.T
  , clxn :: Clxn.T
  , global :: Global.T
  , local :: Local.T
  } deriving (Eq, Ord, Show)
