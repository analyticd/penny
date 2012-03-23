-- | Copper - the Penny parser
module Penny.Copper (
  -- * Parsing a ledger
  Ledger(Ledger),
  ledger,

  -- * Items
  I.Item(Transaction, Price, Comment, BlankLine),

  -- * Filename
  Filename(Filename),

  -- * Radix and grouping
  Q.RadGroup,
  Q.periodComma, Q.periodSpace, Q.commaPeriod, Q.commaSpace,
  
  -- * Default time zone
  DT.DefaultTimeZone(DefaultTimeZone)) where

import Control.Applicative ((<$>))
import Text.Parsec ( manyTill, eof )
import Text.Parsec.Text ( Parser )

import Penny.Lincoln.Meta ( Line, Filename(..) )
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
