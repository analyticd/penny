{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Amount where

import Control.Lens
import Penny.Commodity
import Penny.Decimal

import Text.Show.Pretty (PrettyVal)
import GHC.Generics (Generic)

data Amount = Amount
  { _commodity :: Commodity
  , _qty :: Decimal
  } deriving (Show, Generic)

instance PrettyVal Amount

makeLenses ''Amount
