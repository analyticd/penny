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
drCr = let
  dr = do
    void (char 'D' <|> char 'd')
    void (char 'r') <|> void (string "ebit")
    return L.Debit
  cr = do
    void (char 'C' <|> char 'c')
    void (optional (char 'r' >> optional (string "edit")))
    return L.Credit
  in dr <|> cr

entry :: Q.RadGroup -> Parser (L.Entry, L.Format)
entry rg = f <$> lexeme drCr <*> amount rg <?> e where
  f dc (am, fmt) = (L.Entry dc am, fmt)
  e = "entry"

render ::
  (Q.GroupingSpec, Q.GroupingSpec)
  -> Q.RadGroup
  -> L.Format
  -> L.Entry
  -> Maybe X.Text
render gs rg f (L.Entry dc a) = do
  amt <- A.render gs rg f a
  let dcTxt = X.singleton $ case dc of
        L.Debit -> '<'
        L.Credit -> '>'
  return $ X.append (X.snoc dcTxt ' ') amt
