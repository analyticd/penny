module Penny.Copper.Types where

import Control.Applicative (Applicative (pure))
import Data.Functor ((<$>))
import qualified Data.Text as X
import qualified Data.Traversable as T
import qualified Penny.Lincoln as L
import qualified Data.Monoid as M

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

data Item
  = BlankLine
  | IComment Comment
  | PricePoint L.PricePoint
  | Transaction (L.Transaction)
  deriving Show

mapItem
  :: (Comment -> Comment)
  -> (L.PricePoint -> L.PricePoint)
  -> (L.Transaction -> L.Transaction)
  -> Item
  -> Item
mapItem fc fp ft i = case i of
  BlankLine -> BlankLine
  IComment c -> IComment $ fc c
  PricePoint p -> PricePoint $ fp p
  Transaction t -> Transaction $ ft t

mapItemA
  :: Applicative a
  => (Comment -> a Comment)
  -> (L.PricePoint -> a L.PricePoint)
  -> (L.Transaction -> a (L.Transaction))
  -> Item
  -> a (Item)
mapItemA fc fp ft i = case i of
  BlankLine -> pure BlankLine
  IComment c -> IComment <$> fc c
  PricePoint p -> PricePoint <$> fp p
  Transaction t -> Transaction <$> ft t

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
