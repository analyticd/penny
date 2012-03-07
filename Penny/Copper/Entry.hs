module Penny.Copper.Entry where

import Control.Applicative ((<$>), (<*>))
import Control.Monad ( void )
import Text.Parsec ( char, string, optional, (<|>), (<?>) )
import Text.Parsec.Text ( Parser )

import Penny.Copper.Amount (amount)
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Util (lexeme)
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Meta (Format)

drCr :: Parser B.DrCr
drCr = let
  dr = do
    void (char 'D' <|> char 'd')
    void (char 'r') <|> void (string "ebit")
    return B.Debit
  cr = do
    void (char 'C' <|> char 'c')
    void (optional (char 'r' >> optional (string "edit")))
    return B.Credit
  in dr <|> cr

entry :: Q.RadGroup -> Parser (B.Entry, Format)
entry rg = f <$> lexeme drCr <*> amount rg <?> e where
  f dc (am, fmt) = (B.Entry dc am, fmt)
  e = "entry"
