-- | Copper - the Penny parser
module Penny.Copper (
  Ledger(Ledger),
  ledger,
  parseTransactions,
  Item(Transaction, Price),
  Q.RadGroup,
  Q.periodComma, Q.periodSpace, Q.commaPeriod, Q.commaSpace,
  Filename(Filename),
  DT.DefaultTimeZone(DefaultTimeZone)) where

import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe ( catMaybes )
import Data.Text ( Text, unpack )
import Text.Parsec ( manyTill, eof )
import qualified Text.Parsec as Parsec
import Text.Parsec.Error ( ParseError )
import Text.Parsec.Text ( Parser )

import Penny.Lincoln.Boxes (TransactionBox, PriceBox)
import Penny.Lincoln.Meta ( Line, Filename(Filename) )
import qualified Penny.Copper.Qty as Q
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Item as I

data Ledger =
  Ledger [(Line, I.Item)]
  deriving Show

ledger ::
  Filename
  -> DT.DefaultTimeZone
  -> Q.RadGroup
  -> Parser Ledger
ledger fn dtz rg =
  Ledger
  <$> manyTill (I.itemWithLineNumber fn dtz rg) eof

data Item =
  Transaction TransactionBox
  | Price PriceBox
  deriving Show

parseTransactions ::
  DT.DefaultTimeZone
  -> Q.RadGroup
  -> Filename
  -> Text
  -> Ex.Exceptional ParseError [Item]
parseTransactions dtz rg f@(Filename fn) t =
  case Parsec.parse (ledger f dtz rg) (unpack fn) t of
    (Left e) -> Ex.Exception e
    (Right (Ledger g)) -> let
      pr (_, i) = case i of
        (I.Transaction tr) -> Just (Transaction tr)
        (I.Price price) -> Just (Price price)
        _ -> Nothing
      in return . catMaybes . map pr $ g
