module Penny.Copper.Item (
  itemWithLineNumber,
  Item(Transaction, Price, CommentItem, BlankLine),
  Line(unLine),
  render
  ) where

import Control.Applicative ((<$>), (<*>), (<$))
import qualified Data.Text as X
import Text.Parsec (getPosition, sourceLine, (<|>),
                    (<?>))
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Comments as C
import qualified Penny.Lincoln as L
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Price ( price )
import qualified Penny.Copper.Price as P
import qualified Penny.Copper.Transaction as T
import Penny.Copper.Transaction ( transaction )
import Penny.Copper.Util (eol)


-- | An Item is used both to hold the result of parsing an item from a
-- file and for rendering. It is parameterized on two types: the
-- metadata type for the TopLine, and the metadata type for the
-- Posting.
data Item = Transaction L.Transaction
          | Price L.PricePoint
          | CommentItem C.Comment
          | BlankLine
          deriving Show

newtype Line = Line { unLine :: Int }
               deriving (Eq, Show)

itemWithLineNumber :: Parser (Line, Item)
itemWithLineNumber = (,)
  <$> ((Line . sourceLine) <$> getPosition)
  <*> parseItem

parseItem :: Parser Item
parseItem = let
   bl = BlankLine <$ eol <?> "blank line"
   t = Transaction <$> transaction
   p = Price <$> price
   c = CommentItem <$> C.comment
   in (bl <|> t <|> p <|> c)

render
  :: (Q.GroupingSpec, Q.GroupingSpec)
  -> Item
  -> Maybe X.Text
render gs i = case i of
  Transaction t -> T.render gs t
  Price pp -> P.render gs pp
  CommentItem c -> C.render c
  BlankLine -> Just $ X.singleton '\n'

