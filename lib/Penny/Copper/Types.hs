module Penny.Copper.Types where

import qualified Data.Text as X
import qualified Penny.Lincoln as L

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

data Item = BlankLine
          | IComment Comment
          | PricePoint L.PricePoint
          | Transaction L.Transaction
          deriving Show

newtype Ledger = Ledger { unLedger :: [Item] }
        deriving Show
