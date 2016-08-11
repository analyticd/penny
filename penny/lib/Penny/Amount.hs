{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Amount where

import Control.Lens
import Penny.Commodity
import Penny.Decimal
import Penny.Pretty

import Text.Show.Pretty (PrettyVal)
import qualified Text.Show.Pretty as Pretty
import GHC.Generics (Generic)

data Amount = Amount
  { _commodity :: Commodity
  , _qty :: Decimal
  } deriving (Show, Generic)

instance PrettyVal Amount where
  prettyVal (Amount cy q) = Pretty.Rec "Amount"
    [ ("_commodity", prettyText cy)
    , ("_qty", Pretty.prettyVal q)
    ]

makeLenses ''Amount
