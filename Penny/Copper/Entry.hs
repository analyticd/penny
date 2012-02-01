module Penny.Parser.Entry where

import Control.Monad ( void )
import Text.Parsec ( char, string, optional, (<|>), many1 )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits.Commodity as C
import qualified Penny.Bits.Entry as E
import qualified Penny.Parser.Amount as A
import qualified Penny.Parser.Qty as Q
import qualified Penny.Reports as R


drCr :: Parser E.DrCr
drCr = let
  dr = do
    void (char 'D')
    void (char 'r') <|> void (string "ebit")
    return E.Debit
  cr = do
    void (string "Cr")
    void (optional (string "edit"))
    return E.Credit
  in dr <|> cr

entry ::
  Q.Radix
  -> Q.Separator
  -> Parser (E.Entry, (C.Commodity, R.CommodityFmt))
entry rad sep = do
  dc <- drCr
  void $ many1 (char ' ')
  (am, p) <- A.amount rad sep
  let e = E.Entry dc am
  return (e, p)
