module Penny.Copper.Types where

import Control.Applicative (Applicative (pure))
import Data.Functor ((<$>))
import qualified Data.Text as X
import qualified Data.Traversable as T
import qualified Penny.Lincoln as L
import qualified Data.Monoid as M

data Item
  = BlankLine
  | IComment Comment
  | PricePoint L.PricePoint
  | Transaction (L.Transaction)
  deriving Show

newtype Ledger = Ledger { unLedger :: [Item] }
        deriving Show

mapLedger :: (Item -> Item) -> Ledger -> Ledger
mapLedger f (Ledger is) = Ledger $ map f is

mapLedgerA
  :: Applicative a
  => (Item -> a (Item))
  -> Ledger
  -> a (Ledger)
mapLedgerA f (Ledger is) = Ledger <$> T.traverse f is

instance M.Monoid (Ledger) where
  mempty = Ledger []
  mappend (Ledger x) (Ledger y) = Ledger (x ++ y)
