module Penny.Copper.Item where

import Control.Applicative ((<$>), (<*>), liftA)
import Control.Monad ( liftM )
import Text.Parsec (getPosition, sourceLine, (<|>), char)
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Comments.Multiline as CM
import qualified Penny.Copper.Comments.SingleLine as CS
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Meta as M
import qualified Penny.Copper.Qty as Q
import Penny.Copper.Price ( price )
import Penny.Copper.Transaction ( transaction )
import Penny.Lincoln.Boxes (TransactionBox, PriceBox)


data Item = Transaction (TransactionBox M.TransactionMeta M.PostingMeta)
          | Price (PriceBox M.PriceMeta)
          | Multiline CM.Multiline
          | SingleLine CS.Comment
          | BlankLine
          deriving Show

itemWithLineNumber ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser (M.Line, Item)
itemWithLineNumber fn dtz rad sep = (,)
  <$> liftA (M.Line . sourceLine) getPosition
  <*> parseItem fn dtz rad sep

parseItem ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser Item
parseItem fn dtz rad sep = let
   bl = char '\n' >> return BlankLine
   t = liftM Transaction $ transaction fn dtz rad sep
   p = liftM Price $ price dtz rad sep
   cm = liftM Multiline CM.multiline
   co = liftM SingleLine CS.comment
   in (bl <|> t <|> p <|> cm <|> co)
