module Penny.Copper.Interface where

import qualified Penny.Steel.Sums as S
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

data ParsedTxn = ParsedTxn
  { ptParsedTopLine :: ParsedTopLine
  , ptEnts :: L.Ents (L.PostingCore, L.PostingLine)
  } deriving Show

data BlankLine = BlankLine

newtype Comment = Comment { unComment :: X.Text }
  deriving (Eq, Show)

type ParsedItem = S.S4 BlankLine Comment L.PricePoint ParsedTxn

type Parser
  = String
  -- ^ Filename of the file to be parsed
  -> IO (L.Filename, [ParsedItem])
