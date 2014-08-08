module Penny.TopLine where

import Penny.Common
import Penny.Serial
import Penny.DateTime

data TopLine = TopLine
  { tlDateTime :: DateTime
  , tlMemo :: Memo
  , tlNumber :: Number
  , tlFlag :: Flag
  , tlPayee :: Payee
  , tlLocation :: Location
  , tlClxn :: Clxn
  , tlGlobalSer :: Serial
  , tlClxnSer :: Serial
  } deriving (Eq, Ord, Show)

