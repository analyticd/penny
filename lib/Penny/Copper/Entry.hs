module Penny.Copper.Entry (entry, render) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad ( void )
import qualified Data.Text as X
import Text.Parsec ( char, string, optional, (<|>), (<?>) )
import Text.Parsec.Text ( Parser )

import Penny.Copper.Amount (amount)
import qualified Penny.Copper.Amount as A
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Util (lexeme)
import qualified Penny.Lincoln as L

drCr :: Parser L.DrCr
drCr = dr <|> cr
  where
    dr = do
      void (char 'D' <|> char 'd')
      void (char 'r') <|> void (string "ebit")
      return L.Debit
    cr = do
      void (char 'C' <|> char 'c')
      void (optional (char 'r' >> optional (string "edit")))
      return L.Credit


entry :: Parser (L.Entry, L.Format)
entry = f <$> lexeme drCr <*> amount <?> e where
  f dc (am, fmt) = (L.Entry dc am, fmt)
  e = "entry"

