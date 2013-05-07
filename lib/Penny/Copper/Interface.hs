module Penny.Copper.Interface where

import qualified Penny.Lincoln as L

data ParsedTopLine = ParsedTopLine
  { ptlDateTime :: L.DateTime
  , ptlNumber :: Maybe L.Number
  , ptlFlag :: Maybe L.Flag
  , ptlPayee :: Maybe L.Payee
  , ptlMemo :: Maybe (L.Memo, L.TopMemoLine)
  , ptlTopLineLine :: L.TopLineLine
  } deriving Show

data ParsedTxn = ParsedTxn
  { ptParsedTopLine :: ParsedTopLine
  , ptEnts :: L.Ents (L.PostingCore, L.PostingLine)
  } deriving Show

data ParsedItem
  = PIComment Comment
  | PIPricePoint L.PricePoint
  | PITransaction ParsedTxn
  | PIBlankLine

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

mapItem
  :: (Comment -> Comment)
  -> (L.PricePoint -> L.PricePoint)
  -> (L.Transaction -> L.Transaction)
  -> Item
  -> Item
mapItem fc fp ft i = case i of
  BlankLine -> BlankLine
  IComment c -> IComment $ fc c
  PricePoint p -> PricePoint $ fp p
  Transaction t -> Transaction $ ft t

mapItemA
  :: Applicative a
  => (Comment -> a Comment)
  -> (L.PricePoint -> a L.PricePoint)
  -> (L.Transaction -> a (L.Transaction))
  -> Item
  -> a (Item)
mapItemA fc fp ft i = case i of
  BlankLine -> pure BlankLine
  IComment c -> IComment <$> fc c
  PricePoint p -> PricePoint <$> fp p
  Transaction t -> Transaction <$> ft t

