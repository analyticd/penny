module Penny.Tree.Line where

import qualified Penny.Tree.Comment as Comment
import qualified Penny.Tree.BlankLine as BlankLine
import qualified Penny.Tree.Price as Price
import qualified Penny.Tree.Memo.Transaction as Memo.Transaction
import qualified Penny.Tree.Memo.Posting as Memo.Posting
import qualified Penny.Tree.TopLine as TopLine
import qualified Penny.Tree.Posting as Posting
import Control.Applicative
import Text.Parsec.Text

data T
  = T0 Comment.T
  | T1 BlankLine.T
  | T2 Price.T
  | T3 Memo.Transaction.T
  | T4 TopLine.T
  | T5 Posting.T
  | T6 Memo.Posting.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = fmap T0 Comment.parser
  <|> fmap T1 BlankLine.parser
  <|> fmap T2 Price.parser
  <|> fmap T3 Memo.Transaction.parser
  <|> fmap T4 TopLine.parser
  <|> fmap T5 Posting.parser
  <|> fmap T6 Memo.Posting.parser
