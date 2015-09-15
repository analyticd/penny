{-# LANGUAGE TemplateHaskell #-}
module Penny.Amount where

import Control.Lens
import Penny.Commodity
import Penny.Decimal

data Amount = Amount
  { _commodity :: Commodity
  , _qty :: Decimal
  } deriving Show

makeLenses ''Amount
