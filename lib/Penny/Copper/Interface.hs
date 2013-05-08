module Penny.Copper.Interface where

import qualified Penny.Lincoln as L
import qualified Data.Text as X

data ParsedTopLine = ParsedTopLine
  { ptlDateTime :: L.DateTime
  , ptlNumber :: Maybe L.Number
  , ptlFlag :: Maybe L.Flag
  , ptlPayee :: Maybe L.Payee
  , ptlMemo :: Maybe (L.Memo, L.TopMemoLine)
  , ptlTopLineLine :: L.TopLineLine
  } deriving Show

type ParsedTxn = (ParsedTopLine , L.Ents (L.PostingCore, L.PostingLine))

data BlankLine = BlankLine

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

type ParsedItem
  =  Either ParsedTxn
    (Either L.PricePoint
    (Either Comment BlankLine))

type LedgerItem
  =  Either L.Transaction
    (Either L.PricePoint
    (Either Comment BlankLine))

type Parser
  = String
  -- ^ Filename of the file to be parsed
  -> IO (L.Filename, [ParsedItem])
