module Penny.Copper.Types where

import qualified Data.Text as X
import qualified Penny.Lincoln as L

data Item = BlankLine
          | Comment X.Text
          | PricePoint L.PricePoint
          | Transaction L.Transaction
          deriving Show

newtype Ledger = Ledger { unLedger :: [Item] }
        deriving Show
