module Penny.Core.Posting where

import qualified Penny.Core.Memo as Memo
import qualified Penny.Core.Number as Number
import qualified Penny.Core.Flag as Flag
import qualified Penny.Core.Payee as Payee
import qualified Penny.Core.Tags as Tags
import qualified Penny.Core.Account as Account
import qualified Penny.Core.Location as Location
import qualified Penny.Core.Serial.Global as Global
import qualified Penny.Core.Serial.Local as Local
import qualified Penny.Core.Trio as Trio

data T = T
  { memo :: Memo.T
  , number :: Maybe Number.T
  , flag :: Maybe Flag.T
  , payee :: Maybe Payee.T
  , tags :: Tags.T
  , account :: Account.T
  , location :: Location.T
  , global :: Global.T
  , local :: Local.T
  , trio :: Trio.T
  } deriving (Eq, Ord, Show)
