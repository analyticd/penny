module Penny.Copper (
  ledger,
  parseTransactions,
  Item(Transaction, Price),
  Q.Radix,
  Q.Separator,
  Q.radixAndSeparator,
  Filename(Filename),
  DT.DefaultTimeZone(DefaultTimeZone)) where

import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe ( catMaybes )
import Data.Text ( Text, unpack, pack )
import Text.Parsec ( manyTill, eof )
import qualified Text.Parsec as Parsec
import Text.Parsec.Error ( ParseError )
import Text.Parsec.Text ( Parser )

import Penny.Lincoln.Boxes (TransactionBox, PriceBox)
import Penny.Copper.Meta ( Line, Filename(Filename), TransactionMeta, 
                           PostingMeta, PriceMeta )
import qualified Penny.Copper.Qty as Q
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Item as I

ledger ::
  Filename
  -> DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser [(Line, I.Item)]
ledger fn dtz rad sep = manyTill
                        (I.itemWithLineNumber fn dtz rad sep) eof

data Item =
  Transaction (TransactionBox TransactionMeta PostingMeta)
  | Price (PriceBox PriceMeta)
  deriving Show

parseTransactions ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Filename
  -> Text
  -> Ex.Exceptional ParseError [Item]
parseTransactions dtz rad sep f@(Filename fn) t = let
  p = ledger f dtz rad sep
  in case Parsec.parse p (unpack fn) t of
    (Left e) -> Ex.Exception e
    (Right g) -> let
      pr (_, i) = case i of
        (I.Transaction tr) -> Just (Transaction tr)
        (I.Price price) -> Just (Price price)
        _ -> Nothing
      in return . catMaybes . map pr $ g
