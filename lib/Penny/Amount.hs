{-# LANGUAGE TemplateHaskell #-}
module Penny.Amount where

import Control.Lens
import Penny.Qty
import Penny.Commodity

data Amount = Amount
  { _commodity :: Commodity
  , _qty :: Qty
  } deriving Show

makeLenses ''Amount
