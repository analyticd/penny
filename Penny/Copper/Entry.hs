module Penny.Copper.Entry where

import Control.Monad ( void )
import Text.Parsec ( char, string, optional, (<|>), many1 )
import Text.Parsec.Text ( Parser )

import Penny.Copper.Amount (amount)
import Penny.Copper.Qty (Radix, Separator)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Meta (Format)

drCr :: Parser B.DrCr
drCr = let
  dr = do
    void (char 'D')
    void (char 'r') <|> void (string "ebit")
    return B.Debit
  cr = do
    void (string "Cr")
    void (optional (string "edit"))
    return B.Credit
  in dr <|> cr

entry ::
  Radix
  -> Separator
  -> Parser (B.Entry, Format)
entry rad sep = do
  dc <- drCr
  void $ many1 (char ' ')
  (am, p) <- amount rad sep
  let e = B.Entry dc am
  return (e, p)
