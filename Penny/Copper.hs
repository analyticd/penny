module Penny.Parser where

import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Maybe ( catMaybes )
import Data.Text ( Text, unpack )
import Text.Parsec ( manyTill, eof )
import qualified Text.Parsec as Parsec
import Text.Parsec.Error ( ParseError )
import Text.Parsec.Text ( Parser )

import Penny.Family.Family ( Family )
import Penny.Meta ( Line, Filename, unFilename )
import Penny.Meta ( Meta )
import qualified Penny.Parser.Qty as Q
import qualified Penny.Parser.DateTime as DT
import qualified Penny.Parser.Item as I
import Penny.Posting ( Transaction )

ledger ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser [(Line, I.Item)]
ledger dtz rad sep = manyTill (I.itemWithLineNumber dtz rad sep) eof

parseTransactions ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Filename
  -> Text
  -> Ex.Exceptional ParseError [(Transaction, Family Line Meta)]
parseTransactions dtz rad sep f t = let
  p = ledger dtz rad sep
  in case Parsec.parse p (unpack . unFilename $ f) t of
    (Left e) -> Ex.Exception e
    (Right g) -> let
      pr (_, i) = case i of
        (I.Transaction pair) -> Just pair
        _ -> Nothing
      in return . catMaybes . map pr $ g
