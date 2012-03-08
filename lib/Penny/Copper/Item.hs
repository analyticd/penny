module Penny.Copper.Item where

import Control.Applicative ((<$>), (<*>), (<$))
import Text.Parsec (getPosition, sourceLine, (<|>),
                    (<?>))
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Comments as C
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Price ( price )
import Penny.Copper.Transaction ( transaction )
import Penny.Copper.Util (eol)
import Penny.Lincoln.Boxes (TransactionBox, PriceBox)


data Item = Transaction TransactionBox
          | Price PriceBox
          | Comment C.Comment
          | BlankLine
          deriving Show

itemWithLineNumber ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Q.RadGroup
  -> Parser (M.Line, Item)
itemWithLineNumber fn dtz rg = (,)
  <$> ((M.Line . sourceLine) <$> getPosition)
  <*> parseItem fn dtz rg

parseItem ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Q.RadGroup
  -> Parser Item
parseItem fn dtz rg = let
   bl = BlankLine <$ eol <?> "blank line"
   t = Transaction <$> transaction fn dtz rg
   p = Price <$> price dtz rg
   c = Comment <$> C.comment
   in (bl <|> t <|> p <|> c)
