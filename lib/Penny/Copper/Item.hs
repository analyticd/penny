module Penny.Copper.Item (
  itemWithLineNumber,
  Item(Transaction, Price, CommentItem, BlankLine),
  render
  ) where

import Control.Applicative ((<$>), (<*>), (<$))
import qualified Data.Text as X
import Text.Parsec (getPosition, sourceLine, (<|>),
                    (<?>))
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Comments as C
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Price ( price )
import qualified Penny.Copper.Price as P
import qualified Penny.Copper.Transaction as T
import Penny.Copper.Transaction ( transaction )
import Penny.Copper.Util (eol)
import Penny.Lincoln.Boxes (TransactionBox, PriceBox)


data Item = Transaction TransactionBox
          | Price PriceBox
          | CommentItem C.Comment
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
   c = CommentItem <$> C.comment
   in (bl <|> t <|> p <|> c)

render ::
  DT.DefaultTimeZone
  -> (Q.GroupingSpec, Q.GroupingSpec)
  -> Q.RadGroup
  -> Item
  -> Maybe X.Text
render dtz gs rg i = case i of
  Transaction t -> do
    T.render dtz gs rg t
  Price p -> do
    pair <- P.unbox p
    P.render dtz gs rg pair
  CommentItem c -> C.render c
  BlankLine -> Just $ X.singleton '\n'
    
