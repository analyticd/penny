module Penny.Parser.Item where

import Control.Monad ( liftM )
import Data.Maybe ( catMaybes )
import Text.Parsec (
  getParserState, sourceLine, statePos, 
  (<|>), char)
import Text.Parsec.Text ( Parser )

import qualified Penny.Bundles as B
import Penny.Family.Family ( Family )
import qualified Penny.Parser.Comments.Multiline as CM
import qualified Penny.Parser.Comments.SingleLine as CS
import qualified Penny.Parser.DateTime as DT
import qualified Penny.Parser.Qty as Q
import Penny.Parser.Price ( price )
import qualified Penny.Parser.Price.Data as PriceData
import Penny.Parser.Transaction ( transaction )
import Penny.Posting ( Transaction )
import qualified Penny.Meta as M

data Item = Transaction (Transaction, Family M.Line M.Meta)
          | Price PriceData.Data
          | Multiline CM.Multiline
          | SingleLine CS.Comment
          | BlankLine
          deriving Show

itemWithLineNumber ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser (M.Line, Item)
itemWithLineNumber dtz rad sep = do
  st <- getParserState
  let currLine = M.Line . sourceLine . statePos $ st
  i <- parseItem dtz rad sep
  return (currLine, i)

parseItem ::
  DT.DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser Item
parseItem dtz rad sep = let
   bl = char '\n' >> return BlankLine
   t = liftM Transaction $ transaction dtz rad sep
   p = liftM Price $ price dtz rad sep
   cm = liftM Multiline CM.multiline
   co = liftM SingleLine CS.comment
   in (bl <|> t <|> p <|> cm <|> co)

itemToChildren :: M.Filename
                  -> Item
                  -> Maybe [B.PostingRecord]
itemToChildren f i = case i of
  (Transaction (t, m)) ->
    Just (B.postingsWithMeta f (B.familyWithMeta t m))
  _ -> Nothing

itemsToChildren ::
  M.Filename
  -> [Item]
  -> [B.PostingRecord]
itemsToChildren f = concat . catMaybes . map (itemToChildren f)
