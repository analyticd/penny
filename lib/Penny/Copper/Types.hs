module Penny.Copper.Types where

import Control.Applicative (Applicative (pure))
import Data.Functor ((<$>))
import qualified Data.Text as X
import qualified Data.Traversable as T
import qualified Penny.Lincoln as L
import qualified Data.Monoid as M

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

data Item tm pm
  = BlankLine
  | IComment Comment
  | PricePoint L.PricePoint
  | Transaction (L.Transaction tm pm)
  deriving Show

mapItem
  :: (Comment -> Comment)
  -> (L.PricePoint -> L.PricePoint)
  -> (L.Transaction tm pm -> L.Transaction tm pm)
  -> Item tm pm
  -> Item tm pm
mapItem fc fp ft i = case i of
  BlankLine -> BlankLine
  IComment c -> IComment $ fc c
  PricePoint p -> PricePoint $ fp p
  Transaction t -> Transaction $ ft t

mapItemA
  :: Applicative a
  => (Comment -> a Comment)
  -> (L.PricePoint -> a L.PricePoint)
  -> (L.Transaction tm pm -> a (L.Transaction tm pm))
  -> Item tm pm
  -> a (Item tm pm)
mapItemA fc fp ft i = case i of
  BlankLine -> pure BlankLine
  IComment c -> IComment <$> fc c
  PricePoint p -> PricePoint <$> fp p
  Transaction t -> Transaction <$> ft t

newtype Ledger tm pm = Ledger { unLedger :: [Item tm pm] }
        deriving Show

mapLedger :: (Item tm pm -> Item tm pm) -> Ledger tm pm -> Ledger tm pm
mapLedger f (Ledger is) = Ledger $ map f is

mapLedgerA
  :: Applicative a
  => (Item tm pm -> a (Item tm pm))
  -> Ledger tm pm
  -> a (Ledger tm pm)
mapLedgerA f (Ledger is) = Ledger <$> T.traverse f is

instance M.Monoid (Ledger tm pm) where
  mempty = Ledger []
  mappend (Ledger x) (Ledger y) = Ledger (x ++ y)
