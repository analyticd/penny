module Penny.Parser (
  DT.DefaultTimeZone ( DT.DefaultTimeZone, DT.unDefaultTimeZone ),
  Q.Radix (Q.unRadix), Q.Separator (Q.unSeparator),
  Q.radixAndSeparator,
  ItemWithLineNumber, 
  ledger ) where

import Text.Parsec ( manyTill, eof )
import Text.Parsec.Text ( Parser )

import qualified Penny.Parser.Qty as Q
import qualified Penny.Parser.DateTime as DT
import Penny.Parser.Item ( ItemWithLineNumber, itemWithLineNumber )

ledger ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser [ItemWithLineNumber]
ledger dtz rad sep = manyTill (itemWithLineNumber dtz rad sep) eof
